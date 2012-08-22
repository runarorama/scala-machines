// Copyright   :  (C) 2012 Rúnar Bjarnason, Dan Doel, Edward Kmett
// License     :  BSD-style (see the file LICENSE)

package com.clarifi.machines

import scalaz._

/**
 * You can construct a `Plan` and then `compile` it to a `Machine`.
 * A `Plan[K, O, A]` is a specification for a pure `Machine` that reads inputs selected by `K`,
 * writes values of type `O`, and has intermediate results of type `A`.
 */
sealed trait Plan[+K <: Covariant, +O, +A] {
  def flatMap[L >: K <: Covariant, P >: O, B](f: A => Plan[L, P, B]): Plan[L, P, B]
  def orElse[L >: K <: Covariant, P >: O, B >: A](p: => Plan[L, P, B]): Plan[L, P, B]

  def map[B](f: A => B): Plan[K, O, B] = flatMap { x => Return(f(x)) }

  def inmap[L <: Covariant](p: K => L): Plan[L, O, A] = this match {
    case r@Return(_)    => r
    case Stop           => Stop
    case Emit(o, next)  => Emit(o, () => next() inmap p)
    case Await(k, s, f) => Await(k andThen (_ inmap p), p(s), () => f() inmap p)
  }

  def outmap[P](f: O => P): Plan[K, P, A] = this match {
    case r@Return(x) => r
    case Emit(o, next) => Emit(f(o), () => next() outmap f)
    case Await(k, success, failure) =>
      Await(k andThen (_ outmap f), success, () => failure() outmap f)
    case Stop => Stop
  }

  def foldMap[B](f: O => B)(implicit M: Monoid[B]): B = this match {
    case Emit(o, next)        => M.append(f(o), next() foldMap f)
    case Await(_, _, failure) => failure() foldMap f
    case _                    => M.zero
  }

  def foldRight[R](z: => R)(f: (O, => R) => R): R = this match {
    case Emit(o, next)        => f(o, next().foldRight(z)(f))
    case Await(_, _, failure) => failure().foldRight(z)(f)
    case _                    => z
  }

  def foldLeft[R](acc: R)(f: (R, O) => R): R = this match {
    case Emit(o, next)        => next().foldLeft(f(acc, o))(f)
    case Await(_, _, failure) => failure().foldLeft(acc)(f)
    case _                    => acc
  }

  def >>[L >: K <: Covariant, P >: O, B](next: => Plan[L, P, B]): Plan[L, P, B] =
    flatMap { _ => next }

  def compile: Machine[K, O] = >>(Stop)

  def repeatedly: Machine[K, O] = {
    lazy val r : Machine[K, O] = this >> r
    r
  }

  def replicateM_(n: Int): Plan[K, O, Unit] = n match {
    case 0 => Return(())
    case n => this >> this.replicateM_(n - 1)
  }

  def iterate[L >: K <: Covariant, P >: O, B >: A](h: B => Plan[L, P, B]): Machine[L, P] =
    this match {
      case Return(x)      => h(x) iterate h
      case Stop           => Stop
      case Emit(o, next)  => Emit(o, () => next() iterate h)
      case Await(k, s, f) => Await(k andThen (_ iterate h), s, () => f() iterate h)
    }

  def andThen[P](p: Process[O, P]): Plan[K, P, A] = p match {
    case Emit(o, next)  => Emit(o, () => this andThen next())
    case Stop           => Stop
    case Await(k, s, f) => this match {
      case r@Return(_)    => r
      case Stop           => Stop andThen f()
      case Emit(o, next)  => next() andThen k(s(o))
      case Await(l, t, g) => Await(l andThen (_ andThen p), t, () => g() andThen p)
    }
  }
}

case class Return[+A](x: A) extends Plan[Nothing, Nothing, A] {
  def flatMap[L >: Nothing <: Covariant, P >: Nothing, B](f: A => Plan[L, P, B])   = f(x)
  def orElse[L >: Nothing <: Covariant, P >: Nothing, B >: A](p: => Plan[L, P, B]) = this
}

case class Emit[+K <: Covariant, +O, +A](
  o: O,
  next: () => Plan[K, O, A]
) extends Plan[K, O, A] {
  def flatMap[L >: K <: Covariant, P >: O, B](f: A => Plan[L, P, B]) =
    Emit(o, () => next() flatMap f)
  def orElse[L >: K <: Covariant, P >: O, B >: A](p: => Plan[L, P, B]) =
    Emit(o, () => next() orElse p)
}

case class Await[+K <: Covariant, Z <: K#Ty, +O, +A](
  k: Z => Plan[K, O, A],
  success: K,
  failure: () => Plan[K, O, A]
) extends Plan[K, O, A] {
  def flatMap[L >: K <: Covariant, P >: O, B](f: A => Plan[L, P, B]) =
    Await(k andThen (_ flatMap f), success, () => failure() flatMap f)
  def orElse[L >: K <: Covariant, P >: O, B >: A](p: => Plan[L, P, B]) =
    Await(k andThen (_ orElse p), success, () => p)
}

case object Stop extends Plan[Nothing, Nothing, Nothing] {
  def flatMap[L >: Nothing <: Covariant, P >: Nothing, B](f: Nothing => Plan[L, P, B]) = this
  def orElse[L >: Nothing <: Covariant, P >: Nothing, B >: Nothing](p: => Plan[L, P, B]) = this
}

object Plan {
  implicit def planInstance[K <: Covariant, O]: MonadPlus[({type λ[α] = Plan[K, O, α]})#λ] =
    new MonadPlus[({type λ[+α] = Plan[K, O, α]})#λ] {
      def bind[A, B](m: Plan[K, O, A])(f: A => Plan[K, O, B]) = m flatMap f
      def point[A](a: => A) = Return(a)
      def empty[A] = Stop
      def plus[A](m1: Plan[K, O, A], m2: => Plan[K, O, A]) = m1 orElse m2
    }

  /** The empty plan that just fails. */
  def fail: () => Plan[Nothing, Nothing, Nothing] = () => Stop

  /** Emits the value `a`. */
  def emit[A](a: A): Plan[Nothing, A, Unit] = Emit(a, () => Return(()))

  /** Awaits an input of type `A`. */
  def await[A]: Plan[S[A], Nothing, A] = Await((a: A) => Return(a), Fun(x => x), fail)

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
  trait Handle[+K <: Covariant, +O] {
    def apply[R >: K#Ty](f: O => R): K
  }

  /** Waits for input on a particular `Handle`. */
  def awaits[K <: Covariant, J, O](f: Handle[K, J]): Plan[K, O, J] =
    Await((a: J) => Return(a), f(x => x), fail)
}
