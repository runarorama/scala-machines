// Copyright   :  (C) 2012 Rúnar Bjarnason, Paul Chiusano, Dan Doel, Edward Kmett
// License     :  BSD-style (see the file LICENSE)

package com.clarifi.machines

import scalaz._
import Plan._

/**
 * A `Process` is a `Machine` that accepts inputs of type `I`
 * and emits outputs of type `O`.
 */
object Process {
  import Machine.ProcessCategory._

  /** A `Process` that starts by emitting the contents of the given `Foldable`. */
  def prepended[F[_], A](as: F[A])(implicit F: Foldable[F]): Process[A, A] =
    traversePlan_(as)(emit) >> id

  /** A `Process` that relays inputs matching the given predicate. */
  def filtered[A](p: A => Boolean): Process[A, A] =
    (for {
      i <- await[A]
      _ <- if (p(i)) emit(i) else Return(())
    } yield ()) repeatedly

  /** Drops the first `n` inputs and relays the rest. */
  def dropping[A](n: Int): Process[A, A] =
    await[A].replicateM_(n) >> id

  /** Relays the first `n` inputs and then stops. */
  def taking[A](n: Int): Process[A, A] =
    (await[A] flatMap emit).replicateM_(n).compile

  /** Relays inputs while they match the predicate `p` and then stops. */
  def takingWhile[A](p: A => Boolean): Process[A, A] =
    await[A] flatMap (v => if (p(v)) emit(v) else Stop) repeatedly

  /** Drops inputs while they match the predicate `p` and relays the rest. */
  def droppingWhile[A](p: A => Boolean): Process[A, A] = {
    lazy val loop: Plan[A => Any, A, Unit] = await[A] flatMap (v => if (p(v)) loop else emit(v))
    loop >> id
  }

  /** Buffers its inputs and relays them in lists of size `n`. */
  def buffered[A](n: Int): Process[A, List[A]] = {
    def go(xs: List[A], c: Int): Plan[A => Any, List[A], Unit] = (xs, c) match {
      case (List(), 0) => Stop
      case (acc, 0) => emit(acc reverse)
      case (acc, n) => for {
        i <- await[A] orElse (emit(acc reverse) >> Stop)
        _ <- go(i :: acc, n - 1)
      } yield ()
    }
    go(List(), n) repeatedly
  }

  /** Groups together consecutive inputs that satisfy the relation `p`. */
  def grouping[A](p: (A, A) => Boolean): Process[A, List[A]] = {
    def collect(acc: List[A], x: A): Process[A, List[A]] =
      await[A] orElse (emit(acc reverse) >> Stop) flatMap { y =>
        if (p(x, y)) collect(y::acc, x)
        else emit(acc reverse) >> collect(List(), y)
      }
    await[A] flatMap (collect(List(), _))
  }
}
