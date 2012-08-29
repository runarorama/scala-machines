// Copyright   :  (C) 2012 Rúnar Bjarnason, Paul Chiusano, Dan Doel, Edward Kmett
// License     :  BSD-style (see the file LICENSE)

package com.clarifi.machines

import scalaz._
import Plan._

import scalaz.syntax.monad._

/**
 * A `Process` is a `Machine` that accepts inputs of type `I`
 * and emits outputs of type `O`.
 */
object Process {
  import Machine.ProcessCategory._

  /** A `Process` given by a function in the obvious way. */
  def apply[A, B](f: A => B): Process[A, B] =
    await[A] flatMap (a => emit(f(a))) repeatedly

  /** A `Process` that reduces with a monoid. */
  def reducer[A, B](r: Reducer[A, B]): Process[A, B] = {
    def go(acc: B): Process[A, B] = for {
      a <- await[A] orElse (emit(acc) >> Stop)
      r <- go(r.snoc(acc, a))
    } yield r
    go(r.zero)
  }

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
  def buffered[A](n: Int): Process[A, Vector[A]] = {
    def go(xs: Vector[A], c: Int): Plan[A => Any, Vector[A], Unit] = (xs, c) match {
      case (Vector(), 0) => Stop
      case (acc, 0) => emit(acc)
      case (acc, n) => for {
        i <- await[A] orElse (emit(acc) >> Stop)
        _ <- go(acc :+ i, n - 1)
      } yield ()
    }
    go(Vector(), n) repeatedly
  }

  /** Groups together consecutive inputs that satisfy the relation `p`. */
  def grouping[A](p: (A, A) => Boolean): Process[A, Vector[A]] = {
    def collect(acc: Vector[A], x: A): Process[A, Vector[A]] =
      await[A] orElse (emit(acc) >> Stop) flatMap { y =>
        if (p(x, y)) collect(acc :+ y, x)
        else emit(acc) >> collect(Vector(), y)
      }
    await[A] flatMap (collect(Vector(), _))
  }
}
