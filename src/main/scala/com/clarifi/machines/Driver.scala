// Copyright   :  (C) 2012 Rúnar Bjarnason, Paul Chiusano, Dan Doel, Edward Kmett
// License     :  BSD-style (see the file LICENSE)

package com.clarifi.machines

import scalaz._
import scalaz.syntax.monoid._

/**
 * A `Driver[M, K]` can step through a machine that requests inputs described by `K`
 * and have monadic effects of type `M` at each step.
 */
trait Driver[M[_], K] { self =>

  /**
   * Responds to a single request of type `K`.
   */
  def apply(k: K): M[Option[Any]]

  /**
   * Drives a machine, responding to each request with `apply`, accumulating the
   * machine's output according to a `Monoid`, and accumulating the effects
   * according to a `Monad`.
   */
  def drive[A](m: Machine[K, A])(implicit A: Monoid[A], M: Monad[M]): M[A] = {
    def go(m: Machine[K, A], z: A): M[A] = m match {
      case Stop => M.pure(z)
      case Emit(a, next) => go(next(), z |+| a)
      case Await(k, s, f) =>
        M.bind(apply(s))(_.map(x => go(k(x), z)).getOrElse(go(f(), z)))
    }
    go(m, A.zero)
  }

  /**
   * A driver of `K` can be composed with a driver of `L` to form a driver of
   * the coproduct of `K` and `L`. The resulting driver can drive a machine
   * that sends both `K` and `L` requests.
   */
  def *[L](d: Driver[M, L]): Driver[M, K \/ L] =
    new Driver[M, K \/ L] {
      def apply(k: K \/ L): M[Option[Any]] =
        k.fold(self(_), d(_))
    }
}

