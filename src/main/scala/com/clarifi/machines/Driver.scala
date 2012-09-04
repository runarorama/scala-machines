// Copyright   :  (C) 2012 Rúnar Bjarnason, Paul Chiusano, Dan Doel, Edward Kmett
// License     :  BSD-style (see the file LICENSE)

package com.clarifi.machines

import scalaz._
import scalaz.syntax.monoid._
import scalaz.syntax.monad._

/**
 * A `Driver[K]` can step through a machine that requests inputs described by `K`
 * and have side-effects at each step.
 */
trait Driver[K] { self =>

  /**
   * Responds to a single request of type `K`.
   */
  def apply(k: K): Option[Any]

  /**
   * Drives a machine, responding to each request with `apply`, accumulating the
   * machine's output according to a `Monoid`, and running side-effects willy nilly.
   */
  def drive[A, B](m: Machine[K, A])(g: A => B)
    (implicit B: Monoid[B]): B = {
    def go(m: Machine[K, A], z: B): B = m match {
      case Stop => z
      case Emit(a, next) =>
        val x = g(a)
        go(next(), z |+| x)
      case Await(k, s, f) => apply(s) match {
        case Some(x) => go(k(x), z)
        case None => go(f(), z)
      }
    }
    go(m, B.zero)
  }

  /**
   * A driver of `K` can be composed with a driver of `L` to form a driver of
   * the coproduct of `K` and `L`. The resulting driver can drive a machine
   * that sends both `K` and `L` requests.
   */
  def *[L](d: Driver[L]): Driver[K \/ L] =
    new Driver[K \/ L] {
      def apply(k: K \/ L): Option[Any] =
        k.fold(self(_), d(_))
    }
}

