package com.clarifi.machines

import scalaz._
import Plan._

/**
 *
 */
object Process {
  import Machine.ProcessCategory._

  def prepended[F[_], A](as: F[A])(implicit F: Foldable[F]): Process[A, A] =
    traversePlan_(as)(emit) before id

  def filtered[A](p: A => Boolean): Process[A, A] =
    (for {
      i <- await[A]
      _ <- if (p(i)) emit(i) else Return(())
    } yield ()) repeatedly

  def dropping[A](n: Int): Process[A, A] =
    planInstance.replicateM_(n, await[A]) before id

  def taking[A](n: Int): Process[A, A] =
    planInstance.replicateM_(n, await[A] flatMap emit) compile

  def takingWhile[A](p: A => Boolean): Process[A, A] =
    await[A] flatMap (v => if (p(v)) emit(v) else Fail) repeatedly

  def droppingWhile[A](p: A => Boolean): Process[A, A] = {
    lazy val loop: Plan[Function1, A, A, Unit] = await[A] flatMap (v => if (p(v)) loop else emit(v))
    loop before id
  }

  def buffered[A](n: Int): Process[A, List[A]] = {
    def go(xs: List[A], c: Int): Plan[Function1, A, List[A], Unit] = (xs, c) match {
      case (List(), 0) => Fail
      case (acc, 0) => emit(acc reverse)
      case (acc, n) => for {
        i <- await[A] orElse (emit(acc reverse) >> Fail)
        _ <- go(i :: acc, n - 1)
      } yield ()
    }
    go(List(), n) repeatedly
  }

  def grouping[A](p: (A, A) => Boolean): Process[A, List[A]] = {
    def collect(acc: List[A], x: A): Plan[Function1, A, List[A], Unit] =
      await[A] orElse (emit(acc reverse) >> Fail) flatMap { y =>
        if (p(x, y)) collect(y::acc, x)
        else emit(acc reverse) >> collect(List(), y)
      }
    await[A] flatMap (collect(List(), _)) compile
  }

  def supply[F[_], A, B](xs: F[A])(p: Process[A, B])(implicit F: Foldable[F]): Process[A, B] =
    prepended(xs) andThen p
}
