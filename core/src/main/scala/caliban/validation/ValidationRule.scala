package caliban.validation

sealed trait ValidationRule

object ValidationRule {
  case object UniqueDirective extends ValidationRule
}
