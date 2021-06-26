package caliban.wrappers.deferred

import caliban.introspection.adt.{__Directive, __DirectiveLocation, __InputValue}
import caliban.schema.Types
import caliban.{GraphQL, GraphQLInterpreter}

trait DeferredQuerySupport {

  private[deferred] val Label = __InputValue(
    "label",
    Some("Used to name the response object in a deferred object"),
    () => Types.string,
    None,
    None
  )

  private[deferred] val deferredDirective = List(
    __Directive(
      "defer",
      Some("""The @defer directive is used to mark a field as available for deferred resolution. When the query is processed the field will be separate from the normal
             |execution flow and will be sent separately.
             |""".stripMargin),
      locations = Set(__DirectiveLocation.FIELD, __DirectiveLocation.FRAGMENT_SPREAD),
      args = List(Label)
    )
  )

  /**
   * Adds deferred query support to a graphql schema.
   */
  def withDeferredQuerySupport[R](original: GraphQL[R]): GraphQL[R] =
    original.withDirectives(deferredDirective) @@ DeferredQueries.deferredQueries

}
