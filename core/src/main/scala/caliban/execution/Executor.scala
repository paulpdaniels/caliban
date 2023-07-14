package caliban.execution

import caliban.CalibanError.ExecutionError
import caliban.ResponseValue._
import caliban.Value._
import caliban._
import caliban.execution.Fragment.IsDeferred
import caliban.parsing.adt._
import caliban.schema.ReducedStep.DeferStep
import caliban.schema.Step._
import caliban.schema.{ ReducedStep, Step, Types }
import caliban.transformers.Transformer
import caliban.wrappers.Wrapper.FieldWrapper
import zio._
import zio.query.{ Cache, ZQuery }
import zio.stream.ZStream

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Executor {

  /**
   * Executes the given query against a schema. It returns either an [[caliban.CalibanError.ExecutionError]] or a [[ResponseValue]].
   * @param request a request object containing all information needed
   * @param plan an execution plan
   * @param fieldWrappers a list of field wrappers
   * @param queryExecution a strategy for executing queries in parallel or not
   */
  def executeRequest[R](
    request: ExecutionRequest,
    plan: Step[R],
    fieldWrappers: List[FieldWrapper[R]] = Nil,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    featureSet: Set[Feature] = Set.empty,
    transformers: List[Transformer[R]] = Nil
  )(implicit trace: Trace): URIO[R, GraphQLResponse[CalibanError]] = {

    val execution                                                          = request.operationType match {
      case OperationType.Query        => queryExecution
      case OperationType.Mutation     => QueryExecution.Sequential
      case OperationType.Subscription => QueryExecution.Sequential
    }
    def collectAll[E, A](as: List[ZQuery[R, E, A]]): ZQuery[R, E, List[A]] =
      execution match {
        case QueryExecution.Sequential => ZQuery.collectAll(as)
        case QueryExecution.Parallel   => ZQuery.collectAllPar(as)
        case QueryExecution.Batched    => ZQuery.collectAllBatched(as)
      }

    def reduceStep(
      step: Step[R],
      currentField: Field,
      arguments: Map[String, InputValue],
      path: List[Either[String, Int]]
    ): ReducedStep[R] =
      transformers.foldLeft(step) { case (step, transformer) =>
        transformer.transformStep.lift(step).getOrElse(step)
      } match {
        case s @ PureStep(value)                =>
          value match {
            case EnumValue(v) =>
              // special case of an hybrid union containing case objects, those should return an object instead of a string
              val obj = mergeFields(currentField, v).collectFirst {
                case f: Field if f.name == "__typename" =>
                  ObjectValue(List(f.alias.getOrElse(f.name) -> StringValue(v)))
                case f: Field if f.name == "_"          =>
                  NullValue
              }
              obj.fold(s)(PureStep(_))
            case _            => s
          }
        case FunctionStep(step)                 => reduceStep(step(arguments), currentField, Map(), path)
        case MetadataFunctionStep(step)         => reduceStep(step(currentField), currentField, arguments, path)
        case ListStep(steps)                    =>
          reduceList(
            steps.zipWithIndex.map { case (step, i) =>
              reduceStep(step, currentField, arguments, Right(i) :: path)
            },
            Types.listOf(currentField.fieldType).fold(false)(_.isNullable)
          )
        case ObjectStep(objectName, fields)     =>
          val filteredFields    = mergeFields(currentField, objectName)
          val (deferred, eager) = filteredFields.partitionMap {
            case f @ Field(name @ "__typename", _, _, alias, _, _, _, directives, _, _, _) =>
              Right((alias.getOrElse(name), PureStep(StringValue(objectName)), fieldInfo(f, path, directives)))
            case f @ Field(name, _, _, alias, _, _, args, directives, _, _, fragment)      =>
              val aliasedName = alias.getOrElse(name)
              val field       = fields
                .get(name)
                .fold(NullStep: ReducedStep[R])(reduceStep(_, f, args, Left(alias.getOrElse(name)) :: path))

              val info = fieldInfo(f, path, directives)

              fragment.collectFirst {
                // The defer spec provides some latitude on how we handle responses. Since it is more performant to return
                // pure fields rather than spin up the defer machinery we return pure fields immediately to the caller.
                case IsDeferred(label) if featureSet(Feature.Defer) && !field.isPure =>
                  (label, (aliasedName, field, info))
              }.toLeft((aliasedName, field, info))
          }

          deferred match {
            case Nil => reduceObject(eager, fieldWrappers)
            case d   =>
              DeferStep(
                reduceObject(eager, fieldWrappers),
                d.groupBy(_._1).toList.map { case (label, labelAndFields) =>
                  val (_, fields) = labelAndFields.unzip
                  reduceObject(fields, fieldWrappers) -> label
                },
                path
              )
          }
        case QueryStep(inner)                   =>
          ReducedStep.QueryStep(
            inner.foldCauseQuery(
              e => ZQuery.failCause(effectfulExecutionError(path, Some(currentField.locationInfo), e)),
              a => ZQuery.succeed(reduceStep(a, currentField, arguments, path))
            )
          )
        case StreamStep(stream)                 =>
          if (request.operationType == OperationType.Subscription) {
            ReducedStep.StreamStep(
              stream
                .mapErrorCause(effectfulExecutionError(path, Some(currentField.locationInfo), _))
                .map(reduceStep(_, currentField, arguments, path))
            )
          } else {
            reduceStep(
              QueryStep(ZQuery.fromZIO(stream.runCollect.map(chunk => ListStep(chunk.toList)))),
              currentField,
              arguments,
              path
            )
          }
        case ProxyStep(makeRequest, dataSource) =>
          reduceStep(
            QueryStep(
              ZQuery
                .fromRequest(makeRequest(transformers.foldLeft(currentField)(Transformer.patchField)))(dataSource)
                .map(responseValueToStep)
            ),
            currentField,
            arguments,
            path
          )
      }

    def responseValueToStep(responseValue: ResponseValue): Step[Any] =
      responseValue match {
        case ResponseValue.ListValue(values)   => Step.ListStep(values.map(responseValueToStep))
        case ResponseValue.ObjectValue(fields) =>
          val typeName = fields.toMap.get("__typename").collect { case StringValue(value) => value }.getOrElse("")
          Step.ObjectStep(typeName, fields.map { case (k, v) => k -> responseValueToStep(v) }.toMap)
        case ResponseValue.StreamValue(stream) => Step.StreamStep(stream.map(responseValueToStep))
        case value: Value                      => PureStep(value)
      }

    def makeQuery(
      step: ReducedStep[R],
      errors: Ref[List[CalibanError]],
      deferred: Ref[List[Deferred[R]]]
    ): ZQuery[R, Nothing, ResponseValue] = {

      def handleError(error: ExecutionError, isNullable: Boolean): ZQuery[Any, ExecutionError, ResponseValue] =
        if (isNullable) ZQuery.fromZIO(errors.update(error :: _)).as(NullValue)
        else ZQuery.fail(error)

      @tailrec
      def wrap(query: ZQuery[R, ExecutionError, ResponseValue], isPure: Boolean)(
        wrappers: List[FieldWrapper[R]],
        fieldInfo: FieldInfo
      ): ZQuery[R, ExecutionError, ResponseValue] =
        wrappers match {
          case Nil             => query
          case wrapper :: tail =>
            val q = if (isPure && !wrapper.wrapPureValues) query else wrapper.wrap(query, fieldInfo)
            wrap(q, isPure)(tail, fieldInfo)
        }

      def loop(step: ReducedStep[R]): ZQuery[R, Nothing, Either[ExecutionError, ResponseValue]] =
        step match {
          case PureStep(value)                               => ZQuery.succeed(Right(value))
          case ReducedStep.ListStep(steps, areItemsNullable) =>
            val queries = steps.map(loop(_).flatMap(_.fold(handleError(_, areItemsNullable), ZQuery.succeed(_))))
            collectAll(queries).map(s => ListValue(s)).either
          case ReducedStep.ObjectStep(steps)                 =>
            val queries = steps.map { case (name, step, info) =>
              wrap(loop(step).flatMap(_.fold(ZQuery.fail(_), ZQuery.succeed(_))), step.isPure)(fieldWrappers, info)
                .foldQuery(handleError(_, info.details.fieldType.isNullable), ZQuery.succeed(_))
                .map(name -> _)
            }
            collectAll(queries).map(f => ObjectValue(f)).either
          case ReducedStep.QueryStep(step)                   =>
            step.foldQuery(
              error => ZQuery.succeed(Left(error)),
              query => loop(query)
            )
          case ReducedStep.StreamStep(stream)                =>
            ZQuery
              .environment[R]
              .map(env =>
                Right(
                  ResponseValue.StreamValue(
                    stream
                      .mapZIO(loop(_).flatMap(_.fold(_ => ZQuery.succeed(NullValue), ZQuery.succeed(_))).run)
                      .provideEnvironment(env)
                  )
                )
              )
          case ReducedStep.DeferStep(obj, nextSteps, path)   =>
            val deferredSteps = nextSteps.map { case (step, label) =>
              Deferred(path, step, label)
            }
            ZQuery.fromZIO(deferred.update(deferredSteps ::: _)) *> loop(obj)
        }
      loop(step).flatMap(_.fold(error => ZQuery.fromZIO(errors.update(error :: _)).as(NullValue), ZQuery.succeed(_)))
    }

    def runQuery(step: ReducedStep[R], cache: Cache) =
      for {
        env          <- ZIO.environment[R]
        deferred     <- Ref.make(List.empty[Deferred[R]])
        errors       <- Ref.make(List.empty[CalibanError])
        query         = makeQuery(step, errors, deferred)
        result       <- query.runCache(cache)
        resultErrors <- errors.get
        defers       <- deferred.get
      } yield
        if (defers.nonEmpty) {
          val stream = (makeDeferStream(defers, cache)
            .mapChunks(chunk => Chunk.single(GraphQLIncrementalResponse(chunk.toList, hasNext = true))) ++ ZStream
            .succeed(GraphQLIncrementalResponse.empty))
            .map(_.toResponseValue)
            .provideEnvironment(env)

          GraphQLResponse(
            StreamValue(ZStream.succeed(result) ++ stream),
            resultErrors.reverse,
            hasNext = Some(true)
          )
        } else GraphQLResponse(result, resultErrors.reverse, hasNext = None)

    def makeDeferStream(
      defers: List[Deferred[R]],
      cache: Cache
    ): ZStream[R, Nothing, Incremental[CalibanError]] = {
      def run(d: Deferred[R]) =
        ZStream.unwrap(runIncrementalQuery(d.step, cache, d.path, d.label).map {
          case (resp, Nil)  =>
            ZStream.succeed(resp)
          case (resp, more) =>
            ZStream.succeed(resp) ++ makeDeferStream(more, cache)
        })

      ZStream.mergeAllUnbounded()(defers.map(run): _*)
    }

    def runIncrementalQuery(
      step: ReducedStep[R],
      cache: Cache,
      path: List[Either[String, Int]],
      label: Option[String]
    ) =
      for {
        deferred     <- Ref.make(List.empty[Deferred[R]])
        errors       <- Ref.make(List.empty[CalibanError])
        query         = makeQuery(step, errors, deferred)
        result       <- query.runCache(cache)
        resultErrors <- errors.get
        defers       <- deferred.get
      } yield (Incremental.Defer(
        result,
        errors = resultErrors.reverse,
        path = ListValue(path.map {
          case Left(s)  => StringValue(s)
          case Right(i) => IntValue(i)
        }.reverse),
        label = label
      )
        -> defers)

    for {
      cache    <- Cache.empty
      reduced   = reduceStep(plan, request.field, Map(), Nil)
      response <- runQuery(reduced, cache)
    } yield response
  }

  private[caliban] def fail(error: CalibanError): UIO[GraphQLResponse[CalibanError]] =
    ZIO.succeed(GraphQLResponse(NullValue, List(error)))

  private[caliban] def mergeFields(field: Field, typeName: String): List[Field] = {
    // ugly mutable code but it's worth it for the speed ;)
    val array = ArrayBuffer.empty[Field]
    val map   = collection.mutable.Map.empty[String, Int]
    var index = 0

    field.fields.foreach { field =>
      if (field._condition.forall(_.contains(typeName))) {
        val name = field.alias.getOrElse(field.name)
        map.get(name) match {
          case None        =>
            // first time we see this field, add it to the array
            array += field
            map.update(name, index)
            index = index + 1
          case Some(index) =>
            // field already existed, merge it
            val f = array(index)
            array(index) = f.copy(fields = f.fields ::: field.fields)
        }
      }
    }

    array.toList
  }

  private def fieldInfo(field: Field, path: List[Either[String, Int]], fieldDirectives: List[Directive]): FieldInfo =
    FieldInfo(field.alias.getOrElse(field.name), field, path, fieldDirectives, field.parentType)

  private def reduceList[R](list: List[ReducedStep[R]], areItemsNullable: Boolean): ReducedStep[R] =
    if (list.forall(_.isInstanceOf[PureStep]))
      PureStep(ListValue(list.asInstanceOf[List[PureStep]].map(_.value)))
    else ReducedStep.ListStep(list, areItemsNullable)

  private def reduceObject[R](
    items: List[(String, ReducedStep[R], FieldInfo)],
    fieldWrappers: List[FieldWrapper[R]]
  ): ReducedStep[R] =
    if (!fieldWrappers.exists(_.wrapPureValues) && items.map(_._2).forall(_.isInstanceOf[PureStep]))
      PureStep(ObjectValue(items.asInstanceOf[List[(String, PureStep, FieldInfo)]].map { case (k, v, _) =>
        (k, v.value)
      }))
    else ReducedStep.ObjectStep(items)

  private def effectfulExecutionError(
    path: List[Either[String, Int]],
    locationInfo: Option[LocationInfo],
    cause: Cause[Throwable]
  ): Cause[ExecutionError] =
    cause.failureOption orElse cause.defects.headOption match {
      case Some(e: ExecutionError) => Cause.fail(e.copy(path = path.reverse, locationInfo = locationInfo))
      case other                   => Cause.fail(ExecutionError("Effect failure", path.reverse, locationInfo, other))
    }

  private implicit class EnrichedListOps[+A](val list: List[A]) extends AnyVal {
    def partitionMap[A1, A2](f: A => Either[A1, A2]): (List[A1], List[A2]) = {
      val l = List.newBuilder[A1]
      val r = List.newBuilder[A2]
      list.foreach { x =>
        f(x) match {
          case Left(x1)  => l += x1
          case Right(x2) => r += x2
        }
      }
      (l.result(), r.result())
    }
  }
}
