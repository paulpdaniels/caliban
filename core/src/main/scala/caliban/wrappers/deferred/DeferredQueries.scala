package caliban.wrappers.deferred

import caliban.CalibanError.ExecutionError
import caliban.ResponseValue.{ListValue, ObjectValue, StreamValue}
import caliban.Value.{BooleanValue, IntValue, NullValue, StringValue}
import caliban.execution.FieldInfo
import caliban.parsing.adt.Directive
import caliban.wrappers.Wrapper.{EffectfulWrapper, FieldWrapper, OverallWrapper}
import caliban.{CalibanError, GraphQLRequest, GraphQLResponse, ResponseValue}
import zio.query.ZQuery
import zio.stream.ZStream
import zio.{Chunk, Ref, ZIO}
import zio.query.Described._

/**
 * A deferred query is one which will supply the response in parts. Where fields which are annotated with a
 *  "@defer" will be returned as a separate payload. The standard way to deliver those subsequent payloads
 *  is by using a multipart/mixed payload which can supply a streaming response.
 */
private[deferred] object DeferredQueries {

  type DeferredField = (String, FieldInfo, ZQuery[Any, ExecutionError, ResponseValue])

  lazy val deferredQueries: EffectfulWrapper[Any] =
    EffectfulWrapper(
      Ref
        .make[Chunk[DeferredField]](Chunk.empty)
        .map(ref => deferredQueriesOverall(ref) |+| deferredQueriesField(ref))
    )

  private case class DeferredPayload(
    label: String,
    path: List[Either[String, Int]],
    data: ResponseValue
  ) {
    def toResponseValue(hasNext: Boolean): ResponseValue =
      ObjectValue(
        List(
          "label"   -> StringValue(label),
          "path"    -> ListValue(path.map(_.fold(StringValue, IntValue(_)))),
          "data"    -> data,
          "hasNext" -> BooleanValue(hasNext)
        )
      )
  }

  object Deferred {
    def unapply(directive: Directive): Option[String] =
      directive.arguments.get("label").collectFirst {
        case StringValue(label) if directive.name == "defer" => label
      }
  }

  private def deferredQueriesField(ref: Ref[Chunk[DeferredField]]): FieldWrapper[Any] = new FieldWrapper[Any] {
    override def wrap[R1](query: ZQuery[R1, ExecutionError, ResponseValue], info: FieldInfo): ZQuery[R1, ExecutionError, ResponseValue] =
      info.directives.collectFirst {
        case Deferred(label) => label
      }.fold[ZQuery[R1, ExecutionError, ResponseValue]](query)(
        directive =>
          for {
            env <- ZQuery.environment[R1]
            _ <- ZQuery.fromEffect(ref.update(_ :+ (directive, info, query.provide(env ? "Deferred"))))
          } yield NullValue
      )
  }

  private def mergeResponses[R](deferred: Chunk[DeferredField]) =
    ZStream
      .mergeAllUnbounded()(deferred.map {
        case (label, info, value) =>
          ZStream
            .fromEffect(value.run)
            .map(DeferredPayload(label, info.path, _))
      }: _*)
      .mapAccum(deferred.size)((remaining, payload) => (remaining - 1, payload.toResponseValue(remaining - 1 > 0)))

  /**
   * Adds the deferred stream to the response extensions. The caller of this should strip the extension before returning the payload
   * and then separately run the stream. Though including the extension should have no effect on the client.
   */
  private def deferredQueriesOverall(ref: Ref[Chunk[DeferredField]]): OverallWrapper[Any] =
    new OverallWrapper[Any] {
      override def wrap[R1](f: GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]]): GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]] =
        (request: GraphQLRequest) =>
          for {
            result         <- f(request)
            deferredFields <- ref.get
            extension = deferredFields.nonEmptyOrElse[List[(String, ResponseValue)]](Nil)(
              fields => List("__deferred" -> StreamValue(mergeResponses(fields)))
            )
          } yield
            result.copy(
              extensions = Some(
                ObjectValue(extension ++ result.extensions.fold(List.empty[(String, ResponseValue)])(_.fields))
              ).filter(_.fields.nonEmpty)
            )
    }

}
