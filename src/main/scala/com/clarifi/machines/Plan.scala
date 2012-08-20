// Copyright   :  (C) 2012 Rúnar Bjarnason, Dan Doel, Edward Kmett
// License     :  BSD-style (see the file LICENSE)

package com.clarifi.machines

import scalaz._

/**
 * You can construct a `Plan` and then `compile` it to a `Machine`.
 * A `Plan[K, I, O, A]` is a specification for a pure `Machine` that reads inputs selected by `K`
 * with types based on `I`, writes values of type `O`, and has intermediate results of type `A`.
 */
trait Plan[+K[-_, +_], -I, +O, +A] {
  def flatMap[L[-X, +Y] >: K[X, Y], J <: I, P >: O, B](
    f: A => Plan[L, J, P, B]): Plan[L, J, P, B]
  def orElse[L[-X, +Y] >: K[X, Y], J <: I, P >: O, B >: A](
    o: => Plan[L, J, P, B]): Plan[L, J, P, B]

  def >>[L[-X, +Y] >: K[X, Y], J <: I, P >: O, B](next: => Plan[L, J, P, B]): Plan[L, J, P, B] = flatMap { _ => next }

  def compile: Machine[K, I, O] = Plan.compileAux(this)

  def before[L[-X,+Y]>:K[X,Y],J<:I,P>:O](m: => Machine[L, J, P]): Machine[L, J, P] =
    Plan.beforeAux(this, m)

  def repeatedly: Machine[K, I, O] = {
    lazy val r : Machine[K, I, O] = this before r
    r
  }
}

/** A pure `Plan`. */
case class Return[+A](a: A) extends Plan[Nothing, Any, Nothing, A] {
  def flatMap[K[-X, +Y] >: Nothing, I <: Any, O >: Nothing, B](
    f: A => Plan[K, I, O, B]): Plan[K, I, O, B] = f(a)
  def orElse[K[-X, +Y] >: Nothing, I <: Any, O >: Nothing, B >: A](
    o: => Plan[K, I, O, B]): Plan[K, I, O, B] = this
}

/** Output a result. */
case class Emit[+K[-_, +_], -I, +O, +A](head: O, tail: () => Plan[K, I, O, A]) extends Plan[K, I, O, A] {
  def flatMap[L[-X, +Y] >: K[X, Y], J <: I, P >: O, B](
    f: A => Plan[L, J, P, B]): Plan[L, J, P, B] =
    Emit(head, () => tail() flatMap f)
  def orElse[L[-X, +Y] >: K[X, Y], J <: I, P >: O, B >: A](
    o: => Plan[L, J, P, B]): Plan[L, J, P, B] =
    Emit(head, () => tail() orElse o)
}

/** Wait for input. */
case class Await[+K[-_, +_], -I, +O, +A, Z](
  k: Z => Plan[K, I, O, A],
  success: K[I, Z],
  failure: () => Plan[K, I, O, A]) extends Plan[K, I, O, A] {
  def flatMap[L[-X, +Y] >: K[X, Y], J <: I, P >: O, B](
    f: A => Plan[L, J, P, B]): Plan[L, J, P, B] =
    Await(k andThen (_ flatMap f), success, () => failure() flatMap f)
  def orElse[L[-X, +Y] >: K[X, Y], J <: I, P >: O, B >: A](
    o: => Plan[L, J, P, B]): Plan[L, J, P, B] =
    Await((x: Z) => k(x) orElse o, success, () => o)
}

/** A plan that fails. */
case object Fail extends Plan[Nothing, Any, Nothing, Nothing] {
  def flatMap[K[-X, +Y] >: Nothing, I <: Any, O >: Nothing, B](
    f: Nothing => Plan[K, I, O, B]): Plan[K, I, O, B] = Fail
  def orElse[K[-X, +Y] >: Nothing, I <: Any, O >: Nothing, A >: Nothing](
    o: => Plan[K, I, O, A]): Plan[K, I, O, A] = Fail
}

object Plan {
  implicit def planInstance[K[-_, +_], I, O, A]: MonadPlus[({type λ[+α] = Plan[K, I, O, α]})#λ] =
    new MonadPlus[({type λ[+α] = Plan[K, I, O, α]})#λ] {
      def bind[A, B](m: Plan[K, I, O, A])(f: A => Plan[K, I, O, B]) = m flatMap f
      def point[A](a: => A) = Return(a)
      def empty[A] = Fail
      def plus[A](m1: Plan[K, I, O, A], m2: => Plan[K, I, O, A]) = m1 orElse m2
    }

  /** The empty plan that just fails. */
  def fail: () => Plan[Nothing, Any, Nothing, Nothing] = () => Fail

  /** Emits the value `a`. */
  def emit[A](a: A): Plan[Nothing, Any, A, Unit] = Emit(a, () => Return(()))

  /** Awaits an input of type `A`. */
  def await[A]: Plan[Function1, A, Nothing, A] = Await((a: A) => Return(a), x => x, fail)

  /** A natural transformation from `K[O, _]` to `L[I, _]` */
  trait Fitting[-K[-_, +_], +L[-_, +_], -I, +O] {
    def apply[R](f: K[O, R]): L[I, R]
  }

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
  type Handle[+K[-_, +_], -I, +O] = Fitting[Function1, K, I, O]

  /** Waits for input on a particular `Handle`. */
  def awaits[K[-_, +_], I, O, J](f: Handle[K, I, J]): Plan[K, I, O, J] =
    Await((a: J) => Return(a), f(x => x), fail)

  // Not sure why these don't work in the class directly, but they don't
  // 1) matching on 'Emit' generates errors,
  // Doing it this way just works for some reason.
  private def compileAux[K[-_, +_], I, O, A](p: Plan[K, I, O, A]): Machine[K, I, O] =
    p match {
      case Return(_)      => Machine(Stop)
      case Emit(h, t)     => Machine(Yield(h, () => t() compile))
      case Await(k, s, f) => Machine(Expect(k andThen (_ compile), s, () => f() compile))
      case Fail           => Machine(Stop)
    }

  private def beforeAux[K[-_, +_], I, O, A](p: Plan[K, I, O, A], m: => Machine[K, I, O]): Machine[K, I, O] =
    p match {
      case Return(_)      => m
      case Emit(h, t)     => Machine(Yield(h, () => beforeAux(t(), m)))
      case Await(k, s, f) => Machine(Expect(k andThen (beforeAux(_, m)), s, () => beforeAux(f(),m)))
      case Fail           => Machine(Stop)
    }
}
