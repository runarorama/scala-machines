package com.clarifi.machines

import scalaz._
import scalaz.syntax.monoid._

trait Driver[M[_], K] { self =>
  def apply(k: K): M[Option[Any]]

  def drive[A](m: Machine[K, A])(implicit A: Monoid[A], M: Monad[M]): M[A] = {
    def go(m: Machine[K, A], z: A): M[A] = m match {
      case Stop => M.pure(z)
      case Emit(a, next) => go(next(), z |+| a)
      case Await(k, s, f) =>
        M.bind(apply(s))(_.map(x => go(k(x), z)).getOrElse(go(f(), z)))
    }
    go(m, A.zero)
  }

  def *[L](d: Driver[M, L]): Driver[M, K \/ L] =
    new Driver[M, K \/ L] {
      def apply(k: K \/ L): M[Option[Any]] =
        k.fold(self(_), d(_))
    }
}


