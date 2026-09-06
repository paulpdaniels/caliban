package caliban.gateway

abstract class GatewayAspect[-R] { self =>
  def @@[R1 <: R](that: GatewayAspect[R1]): GatewayAspect[R1] =
    new GatewayAspect[R1] {
      private[gateway] def apply[R2 <: R1](gateway: Gateway[R2]): Gateway[R2] =
        that(self(gateway))
    }

  private[gateway] def apply[R1 <: R](gateway: Gateway[R1]): Gateway[R1]
}

object GatewayAspect {

  val empty: GatewayAspect[Any] = new GatewayAspect[Any] {
    private[gateway] def apply[R1 <: Any](gateway: Gateway[R1]): Gateway[R1] = gateway
  }

}
