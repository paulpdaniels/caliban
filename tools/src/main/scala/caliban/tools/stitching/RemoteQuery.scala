package caliban.tools.stitching

import caliban.{ GraphQLRequest, InputValue }
import caliban.execution.Field
import caliban.Value._
import caliban.InputValue._
import caliban.Value.FloatValue._
import caliban.Value.IntValue._
import caliban.rendering.{ AstRenderer, RenderingOptions }

case class RemoteQuery(field: Field) { self =>
  def toGraphQLRequest: GraphQLRequest =
    GraphQLRequest(query =
      Some(
        RemoteQuery.QueryRenderer.render(self)
      )
    )
}

case class RemoteMutation(field: Field) { self =>
  def toGraphQLRequest: GraphQLRequest =
    GraphQLRequest(query =
      Some(
        RemoteQuery.QueryRenderer.render(self)
      )
    )
}

object RemoteQuery {
  object QueryRenderer {
    private def withConditionRenderer(renderer: AstRenderer[Field]): AstRenderer[Field] =
      (in: Field, out: StringBuilder, opts: RenderingOptions) =>
        in.condition.fold(renderer.render(in, out, opts)) {
          _.foreach { cond =>
            out
              .append("... on ")
              .append(cond)
              .append(" { ")
            renderer.render(in, out, opts)
            out.append(" }")
          }
        }

    private lazy val fieldRenderer: AstRenderer[Field] = (in: Field, out: StringBuilder, opt: RenderingOptions) => {
      in.alias.foreach(a => out.append(a).append(':').append(' '))
      out.append(in.name)
      in.arguments.map { case (k, v) =>
        val b = new StringBuilder(k).append(':').append(' ')
        inputValueRenderer.render(v, b, opt)
        b
      }.addString(out, "(", ", ", ")")
      in.fields.addString(out, " { ", " ", " }")
    }

    private lazy val inputValueRenderer: AstRenderer[InputValue] =
      (in: InputValue, out: StringBuilder, opt: RenderingOptions) =>
        in match {
          case StringValue(value)      => out.append('"').append(value).append('"')
          case ListValue(values)       =>
            values.map { v =>
              val b = new StringBuilder()
              inputValueRenderer.render(v, b, opt)
              b
            }.addString(out, "[", ",", "]")
          case ObjectValue(fields)     =>
            fields.map { case (k, v) =>
              val b = new StringBuilder(k).append(':').append(' ')
              inputValueRenderer.render(v, b, opt)
              b
            }.addString(out, "{ ", ", ", " }")
          case NullValue               => out.append("null")
          case VariableValue(name)     => out.append('$').append(name)
          case BigDecimalNumber(value) => out.append(value)
          case BigIntNumber(value)     => out.append(value)
          case BooleanValue(value)     => out.append(value)
          case DoubleNumber(value)     => out.append(value)
          case EnumValue(value)        => out.append(value)
          case FloatNumber(value)      => out.append(value)
          case IntNumber(value)        => out.append(value)
          case LongNumber(value)       => out.append(value)
        }

    val mutationRenderer: AstRenderer[RemoteMutation] =
      mutationRenderer(withConditionRenderer(fieldRenderer))

    val queryRenderer: AstRenderer[RemoteQuery] =
      queryRenderer(withConditionRenderer(fieldRenderer))

    def mutationRenderer(fieldRenderer: AstRenderer[Field]): AstRenderer[RemoteMutation] =
      (in: RemoteMutation, out: StringBuilder, opts: RenderingOptions) => {
        out.append("mutation { ")
        fieldRenderer.render(in.field, out, opts)
        out.append(" }")
      }

    def queryRenderer(fieldRenderer: AstRenderer[Field]): AstRenderer[RemoteQuery] =
      (in: RemoteQuery, out: StringBuilder, opts: RenderingOptions) => {
        out.append("query { ")
        fieldRenderer.render(in.field, out, opts)
        out.append(" }")
      }

    def render(r: RemoteMutation): String =
      AstRenderer.render(r)(mutationRenderer)

    def render(r: RemoteQuery): String =
      AstRenderer.render(r)(queryRenderer)
  }
}
