package caliban.wrappers

import caliban.{GraphQLResponse, ResponseValue}
import zio.stream.ZStream
import zio.{URIO, ZIO}

package object deferred extends DeferredQuerySupport {

  implicit class DeferredQueryOps[R, E](val query: URIO[R, GraphQLResponse[E]]) extends AnyVal {
    def asDeferred: URIO[R, (GraphQLResponse[E], ZStream[R, Nothing, ResponseValue])] = {
      query.map { response =>
        val deferredR = response.extensions
      }
    }
  }

}
