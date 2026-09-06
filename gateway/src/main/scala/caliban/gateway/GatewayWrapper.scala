package caliban.gateway

import caliban.GraphQLRequest
import caliban.parsing.adt.OperationType
import sttp.model.Header
import zio.{ Cause, Exit }

/**
 * The vocabulary of the gateway execution lifecycle: the [[GatewayWrapper.Event]]s a gateway reaches and the
 * [[GatewayWrapper.Result]]s they complete with.
 *
 * Events carry bounded metadata only, except where a specific event documents the request data it receives. Attach
 * behaviour to an event with [[PhaseHooks]], either directly via `Gateway#withPhaseHooks` or bundled into a
 * [[GatewayAspect]] applied with `@@`.
 */
object GatewayWrapper {
  sealed trait Outcome extends Product with Serializable {
    def label: String
  }

  object Outcome {
    case object Success         extends Outcome { val label = "success"          }
    case object GraphQLError    extends Outcome { val label = "graphql_error"    }
    case object RequestError    extends Outcome { val label = "request_error"    }
    case object TransportError  extends Outcome { val label = "transport_error"  }
    case object Timeout         extends Outcome { val label = "timeout"          }
    case object LimitExceeded   extends Outcome { val label = "limit_exceeded"   }
    case object InvalidResponse extends Outcome { val label = "invalid_response" }
    case object Cancelled       extends Outcome { val label = "cancelled"        }
    case object InternalError   extends Outcome { val label = "internal_error"   }
  }

  sealed trait CacheResult extends Product with Serializable {
    def label: String
  }

  object CacheResult {
    case object Hit  extends CacheResult { val label = "hit"  }
    case object Miss extends CacheResult { val label = "miss" }
    case object Wait extends CacheResult { val label = "wait" }
  }

  sealed trait AdmissionKind extends Product with Serializable {
    def label: String
  }

  object AdmissionKind {
    case object Request           extends AdmissionKind { val label = "request"            }
    case object Subgraph          extends AdmissionKind { val label = "subgraph"           }
    case object SubscriptionSetup extends AdmissionKind { val label = "subscription_setup" }
    case object SubscriptionEvent extends AdmissionKind { val label = "subscription_event" }
  }

  final case class Result(
    outcome: Outcome,
    operationType: Option[OperationType] = None,
    errorCount: Int = 0,
    statusCode: Option[Int] = None,
    responseBytes: Option[Long] = None
  )

  object Result {
    private[gateway] def fromResponse(response: caliban.GraphQLResponse[_]): Result =
      Result(
        if (response.errors.isEmpty) Outcome.Success else Outcome.GraphQLError,
        errorCount = response.errors.size
      )

    private[gateway] def classifyExit[E, A](exit: Exit[E, A]): Result =
      fromExit(exit)(_ => Result(Outcome.Success), _ => Result(Outcome.InternalError))

    private[gateway] def fromExit[E, A](exit: Exit[E, A])(success: A => Result, failure: E => Result): Result =
      exit match {
        case Exit.Success(value) => success(value)
        case Exit.Failure(cause) => cause.failureOption.fold(fromCause(cause))(failure)
      }

    private[gateway] def fromCause(cause: Cause[_]): Result =
      Result(if (cause.isInterrupted) Outcome.Cancelled else Outcome.InternalError)
  }

  sealed trait Event extends Product with Serializable

  object Event {
    case object SubscriptionSetup                                                 extends Event
    case object SubscriptionEvent                                                 extends Event
    final case class SubscriptionTerminated(reason: String, durationNanos: Long)  extends Event
    final case class SubscriptionAdmission(accepted: Boolean)                     extends Event
    case object SubscriptionOverflow                                              extends Event
    final case class Request(operationName: Option[String])                       extends Event
    case object Routing                                                           extends Event
    final case class SubgraphCall(subgraph: String, operationType: OperationType) extends Event
    final case class Attempt(
      subgraph: String,
      number: Int,
      requestBytes: Long,
      serverAddress: Option[String],
      serverPort: Option[Int]
    ) extends Event
    final case class Retry(subgraph: String, attempt: Int)                        extends Event
    case object Completion                                                        extends Event
    final case class CacheAccess(result: CacheResult)                             extends Event
    final case class Admission(kind: AdmissionKind)                               extends Event

    /**
     * The custom progressive `@override` labels the selected operation reached, and the subset a handler has
     * activated so far. Labels stay inactive unless a handler activates them, so several handlers can each
     * contribute without one clearing another's selection.
     *
     * The gateway resolves built-in `percent(x)` labels itself and ignores anything activated that the operation
     * did not reach.
     */
    final case class OverrideLabels(
      request: GraphQLRequest,
      reached: Set[String],
      active: Set[String] = Set.empty
    ) extends Event {
      def activate(labels: Set[String]): OverrideLabels = copy(active = active ++ labels)
    }
    final case class OutboundHeaders(subgraph: String, headers: List[Header])              extends Event
    final case class AttemptHeaders(subgraph: String, attempt: Int, headers: List[Header]) extends Event
    final case class ObserveOperation(request: GraphQLRequest)                             extends Event
  }

  private[gateway] def operationTypeLabel(operationType: OperationType): String =
    operationType match {
      case OperationType.Query        => "query"
      case OperationType.Mutation     => "mutation"
      case OperationType.Subscription => "subscription"
    }
}
