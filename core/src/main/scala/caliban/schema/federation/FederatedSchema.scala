package caliban.schema.federation

import caliban.CalibanError.ExecutionError
import caliban.ResponseValue.ObjectValue
import caliban.{ GraphQL, InputValue, ResponseValue, RootResolver, Value }
import caliban.introspection.adt.__Type
import caliban.schema.{ ArgBuilder, Types }
import zio.{ RIO, UIO, ZIO }

/**
 * scalar _Any
 * scalar _FieldSet
 *
 * # a union of all types that use the @key directive
 * union _Entity
 *
 * type _Service {
 * sdl: String
 * }
 *
 * extend type Query {
 * _entities(representations: [_Any!]!): [_Entity]!
 * _service: _Service!
 * }
 *
 * directive @external on FIELD_DEFINITION
 * directive @requires(fields: _FieldSet!) on FIELD_DEFINITION
 * directive @provides(fields: _FieldSet!) on FIELD_DEFINITION
 * directive @key(fields: _FieldSet!) on OBJECT | INTERFACE
 *
 * # this is an optional directive discussed below
 * directive @extends on OBJECT | INTERFACE
 */
object FederatedSchema {

  case class _Service(sdl: String)
  case class _Any(value: InputValue.ObjectValue)
  case class AnyArgs(representations: List[InputValue.ObjectValue])
  case class Queries[-R](_service: _Service, _entities: AnyArgs => RIO[R, List[ResponseValue]])

  // For federation we need to accept a base schema and augment it with additional capabilities
  def federate[R](delegate: GraphQL[R], f: (String, InputValue) => RIO[R, ResponseValue]): GraphQL[R] = {
    // First we need to create some new types which are required by the schema
    val _AnyScalar = Types.makeScalar("_Any")
    val _FieldSet  = Types.makeScalar("_FieldSet")
    val _Entity    = Types.makeUnion(Some("_Entity"), None, List.empty)

    val _anyArg: ArgBuilder[_Any] = {
      case obj @ InputValue.ObjectValue(fields) if fields.contains("__typename") => Right(_Any(obj))
      case _                                                                     => Left(ExecutionError("Can't read object type _Any"))
    }

    val resolver = RootResolver(
      Queries(
        _service = _Service(delegate.render),
        _entities = args =>
          ZIO.traversePar(args.representations) { rep =>
            rep.fields.get("__typename") match {
              case Some(Value.StringValue(typename)) =>
                f(typename, rep)
            }
          }
      )
    )

    delegate |+| GraphQL.graphQL(resolver)
  }

}
