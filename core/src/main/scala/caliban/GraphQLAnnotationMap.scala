package caliban

import zio.Tag
import caliban.validation.ValidationRule

final class GraphQLAnnotationMap private (private val map: Map[GraphQLAnnotation[Any], AnyRef]) {
  def annotate[V](key: GraphQLAnnotation[V], value: V): GraphQLAnnotationMap = ???

  def get[V](key: GraphQLAnnotation[V]): V = ???
}

final class GraphQLAnnotation[V] private (
  val identifier: String,
  val initial: V,
  val combine: (V, V) => V,
  private val tag: Tag[V]
) extends Serializable {

  override def equals(that: Any): Boolean = (that: @unchecked) match {
    case that: GraphQLAnnotation[_] => (identifier, tag) == ((that.identifier, that.tag))
  }

  override lazy val hashCode: Int =
    (identifier, tag).hashCode
}

object GraphQLAnnotation {
  def apply[V](identifier: String, initial: V, combine: (V, V) => V)(implicit tag: Tag[V]): GraphQLAnnotation[V] =
    new GraphQLAnnotation(identifier, initial, combine, tag)
  
  def setRule(rule: ValidationRule): GraphQLAnnotation[Set[ValidationRule]] =
    GraphQLAnnotation("validate", Set(rule), _ ++ _) 
}
