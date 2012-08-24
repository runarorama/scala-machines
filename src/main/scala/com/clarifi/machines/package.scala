package com.clarifi

import scalaz._
import syntax.foldable._

package object machines {
  import Plan._

  type T[-A, -B] = (A => Any) \/ (B => Any)

  /**
   * Many combinators are parameterized on the choice of `Handle`.
   * This acts like an input stream selector.
   *
   * For example:
   * {{{
   * L : Handle[Merge, (A, B), A]
   * R : Handle[Merge, (A, B), B]
   * }}}
   */
  type Handle[+K, +O] = (O => Any) => K

  type Machine[+K, +O] = Plan[K, O, Nothing]

  type Process[-I, +O] = Machine[I => Any, O]

  sealed class ProcessW[-I, +O](p: Process[I, O]) {
    import Process._
    import Machine.ProcessCategory._

    def cap(r: Source[I]): Source[O] =
      r andThen p

    def supply[F[+_]](xs: F[I])(implicit F: Foldable[F]): Process[I, O] =
      prepended(xs) andThen p
  }
  implicit def processw[I, O](p: Process[I, O]): ProcessW[I, O] = new ProcessW(p)

  type Source[+O] = Machine[Nothing, O]

  type Tee[-A, -B, +C] = Machine[T[A, B], C]
  sealed class TeeW[-I, -J, +O](t: Tee[I, J, O]) {
    import Tee._
    import Machine.ProcessCategory._

    def addL[A](p: Process[A, I]): Tee[A, J, O] =
      tee(p, id, t)

    def addR[B](p: Process[B, J]): Tee[I, B, O] =
      tee(id, p, t)

    def capL[A](s: Source[I]): Process[J, O] =
      addL(s) inmap cappedT

    def capR[B](s: Source[J]): Process[I, O] =
      addR(s) inmap cappedT
  }
  implicit def teew[I, J, O](tee: Tee[I, J, O]): TeeW[I, J, O] = new TeeW(tee)

  type Wye[-I, -J, +O] = Machine[These[I => Any, J => Any], O]

  def traversePlan_[F[_], K, O, A](as: F[A])(f: A => Plan[K, O, Unit])(implicit F: Foldable[F]): Plan[K, O, Unit] =
    as.traverse_[({type λ[α] = Plan[K, O, α]})#λ](f)(planInstance[K, O])

}

