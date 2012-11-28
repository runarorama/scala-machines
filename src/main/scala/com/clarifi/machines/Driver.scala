// Copyright   :  (C) 2012 Rúnar Bjarnason, Paul Chiusano, Dan Doel, Edward Kmett
// License     :  BSD-style (see the file LICENSE)

package com.clarifi.machines

import scalaz._
import scalaz.syntax.monoid._
import scalaz.syntax.monad._
import scalaz.Id._

/**
 * A `Driver[K]` can step through a machine that requests inputs described by `K`
 * and have side-effects at each step.
 */
trait Driver[M[+_], K] { self =>

  implicit def M: Monad[M]

  /**
   * Responds to a single request of type `K`.
   */
  def apply(k: K): M[Option[Any]]

  // def apply(k: K): Option[Any] // with side effects!!!
  // def drive[A,B](m: Machine[K,A])(g: A => B)(implicit B: Monoid[B]): B // with side effects!
  // def apply[B](m: Machine[K, B]): (M[Iterator[B]], M[Unit]) =
    // defined in terms of signals

  /**
   * Drives a machine, responding to each request with `apply`, accumulating the
   * machine's output according to a `Monoid`, and running side-effects willy nilly.
   */
  def drive[A, B](m: Machine[K, A])(g: A => M[B])(implicit B: Monoid[B]):
  M[B] = {
    def go(m: Machine[K, A], z: B): M[B] = m match {
      case Stop => z.pure[M]
      case Emit(a, k) =>
        val next = k()
        g(a) flatMap (x => go(next, z |+| x))
      case Await(k, s, f) =>
        apply(s).map(_.map(k).getOrElse(f())).flatMap(go(_, z))
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
      val M = self.M
      def apply(k: K \/ L): M[Option[Any]] =
        k.fold(self(_), d(_))
    }
}

object Driver {

  private def driveId[A, B, K](drv: K => Option[Any]
                             )(m: Machine[K, A])(g: A => B)(implicit B: Monoid[B]): B = {
    @annotation.tailrec
    def go(acc: B, m: Machine[K,A]): B = {
      m match {
        case Stop => acc
        case Emit(h, t) =>
          val r = g(h) // we are left folding, so Monoid should be strict, otherwise stack overflow / memory leak
          go(B.append(acc, r), t())
        case Await(recv, k, fb) => go(acc, drv(k) map recv getOrElse fb())
      }
    }
    go(B.zero, m)
  }

  /** Creates a driver from a (possibly) impure function, specializing
    * drive to Id to avoid stack overflows.
    */
  def Id[K](drv: K => Option[Any]): Driver[Id,K] = new Driver[Id,K] {self =>
    val M = Monad[Id]
    def apply(k: K) = drv(k)
    override def drive[A, B](m: Machine[K, A])(g: A => B)(implicit B: Monoid[B]): B =
      driveId(drv)(m)(g)
    override def *[L](d: Driver[Id, L]): Driver[Id, K \/ L] = Id(_ fold (self.apply, d.apply))
  }
}

