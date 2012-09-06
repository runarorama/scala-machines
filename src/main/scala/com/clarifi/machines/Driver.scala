// Copyright   :  (C) 2012 Rúnar Bjarnason, Paul Chiusano, Dan Doel, Edward Kmett
// License     :  BSD-style (see the file LICENSE)

package com.clarifi.machines

import scalaz._
import scalaz.syntax.monoid._
import scalaz.syntax.monad._

/**
 * A `Driver[M, K]` can step through a machine that requests inputs described by `K`
 * and have monadic effects of type `M` at each step.
 */
trait Driver[M[+_], K] { self =>

  /**
   * Responds to a single request of type `K`.
   */
  def apply(k: K): M[Option[Any]]

  /**
   * Drives a machine, responding to each request with `apply`, accumulating the
   * machine's output according to a `Monoid`, and accumulating the effects
   * according to a `Monad`.
   */
  def drive[A, B](m: Machine[K, A])(g: A => M[B])
    (implicit B: Monoid[B], M: Monad[M]): M[B] = {
    def go(m: Machine[K, A], z: B): M[B] = m match {
      case Emit(a, k) => {
        val next = k()
        g(a) flatMap (x => go(next, z |+| x))
      }
      case Await(k, s, f) =>
        apply(s).map(_.map(k).getOrElse(f())).flatMap(go(_, z))
      case Stop => z.pure[M]
    }
    go(m, B.zero)
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

