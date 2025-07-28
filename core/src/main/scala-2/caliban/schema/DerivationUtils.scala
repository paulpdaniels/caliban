package caliban.schema

import caliban.schema.Annotations.{ GQLExtend, GQLValueType }
import magnolia1.ReadOnlyCaseClass

private object DerivationUtils {

  def isValueType[F[_]](ctx: ReadOnlyCaseClass[F, ?]): Boolean =
    (ctx.isValueClass || ctx.annotations.exists(_.isInstanceOf[GQLValueType])) && ctx.parameters.nonEmpty

  def isExtended[F[_]](ctx: ReadOnlyCaseClass[F, ?]): Boolean =
    ctx.parameters.exists(_.annotations.exists(_.isInstanceOf[GQLExtend]))

}
