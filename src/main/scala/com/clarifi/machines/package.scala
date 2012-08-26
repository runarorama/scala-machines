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
   * For example, to select one of two streams, on the `left` or `right`:
   * {{{
   * left  : Handle[T[A, Any], A]
   * right : Handle[T[Any, A], B]
   * }}}
   */
  type Handle[+K, +O] = (O => Any) => K

  /**
   * A machine that requests inputs described by `K` and emits values of type `O`.
   */
  type Machine[+K, +O] = Plan[K, O, Nothing]

  /**
   * A machine that requests values of type `I` and emits values of type `O`.
   */
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

  /**
   * A machine that emits values of type `O` without taking any input.
   */
  type Source[+O] = Machine[Nothing, O]

  /**
   * A machine that can request values of type `A` on the left,
   * request values of type `B` on the right, and emit values of type `C`.
   */
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

  /**
   * Same as a `Tee` except it can indicate that it has no preference whether it
   * receives an `I` on the left or a `J` on the right.
   */
  type Wye[-I, -J, +O] = Machine[These[I => Any, J => Any], O]

  def traversePlan_[F[_], K, O, A](as: F[A])(f: A => Plan[K, O, Unit])(implicit F: Foldable[F]): Plan[K, O, Unit] =
    as.traverse_[({type λ[α] = Plan[K, O, α]})#λ](f)(planInstance[K, O])

}

