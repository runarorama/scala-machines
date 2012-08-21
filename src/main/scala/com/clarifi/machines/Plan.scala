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

  def map[B](f: A => B): Plan[K, I, O, B] = flatMap(x => Return(f(x)))

  def iomap[L[-X,+Y] >: K[X, Y], J, P](f: J => I, g: O => P)(implicit L: Profunctor[L]): Plan[L, J, P, A] =
    Plan.iomapAux[L, J, I, O, P, A](this)(f, g)

  def outmap[P](f: O => P): Plan[K, I, P, A] = Plan.outmapAux(this)(f)

  def inmap[L[-X,+Y] >: K[X, Y], J](f: J => I)(implicit L: Profunctor[L]): Plan[L, J, O, A] =
    Plan.inmapAux[L, J, I, O, A](this)(f)

  def foldMap[B](f: O => B)(implicit M: Monoid[B]): B = Plan.foldMapAux(this)(f)

  def foldRight[R](z: => R)(f: (O, => R) => R): R = Plan.foldRightAux(this)(z, f)

  def foldLeft[R](acc: R)(f: (R, O) => R): R = Plan. foldLeftAux(this)(acc, f)

  def >>[L[-X, +Y] >: K[X, Y], J <: I, P >: O, B](next: => Plan[L, J, P, B]): Plan[L, J, P, B] = flatMap { _ => next }

  def compile: Machine[K, I, O] = >>(Stop)

  def repeatedly: Machine[K, I, O] = {
    lazy val r : Machine[K, I, O] = this >> r
    r
  }

  def iterate[L[-X,+Y] >: K[X, Y], J <: I, P >: O, B >: A](f: B => Plan[L, J, P, B]): Machine[L, J, P] =
    Plan.iterateAux(f)(this)

  def fitting[L[-_, +_], J](g: Fitting[K, L, J, I]): Plan[L, J, O, A] = Plan.fittingAux(this,g)

  def andThen[P](p: Process[O, P]): Plan[K, I, P, A] = Plan.andThenAux(this, p)
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
case object Stop extends Plan[Nothing, Any, Nothing, Nothing] {
  def flatMap[K[-X, +Y] >: Nothing, I <: Any, O >: Nothing, B](
    f: Nothing => Plan[K, I, O, B]): Plan[K, I, O, B] = Stop
  def orElse[K[-X, +Y] >: Nothing, I <: Any, O >: Nothing, A >: Nothing](
    o: => Plan[K, I, O, A]): Plan[K, I, O, A] = Stop
}

object Plan {
  implicit def planInstance[K[-_, +_], I, O]: MonadPlus[({type λ[α] = Plan[K, I, O, α]})#λ] =
    new MonadPlus[({type λ[+α] = Plan[K, I, O, α]})#λ] {
      def bind[A, B](m: Plan[K, I, O, A])(f: A => Plan[K, I, O, B]) = m flatMap f
      def point[A](a: => A) = Return(a)
      def empty[A] = Stop
      def plus[A](m1: Plan[K, I, O, A], m2: => Plan[K, I, O, A]) = m1 orElse m2
    }

  /** The empty plan that just fails. */
  def fail: () => Plan[Nothing, Any, Nothing, Nothing] = () => Stop

  /** Emits the value `a`. */
  def emit[A](a: A): Plan[Nothing, Any, A, Unit] = Emit(a, () => Return(()))

  /** Awaits an input of type `A`. */
  def await[A]: Plan[Function1, A, Nothing, A] = Await((a: A) => Return(a), x => x, fail)

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

  // These `Aux` functions crash in 2.9.2 if they are defined directly on the class.
  // Related to a bug fixed in 2.10.0 regarding higher-kinded type checking
  private def inmapAux[K[-_, +_], J, I, O, A](p: Plan[K, I, O, A])(f: J => I)(implicit K: Profunctor[K]): Plan[K, J, O, A] =
    p match {
      case Return(x) => Return(x)
      case Emit(o, xs) => Emit(o, () => inmapAux(xs())(f))
      case Await(k, succ, fail) => Await(k andThen (inmapAux(_)(f)), K.lmap(succ)(f), () => inmapAux(fail())(f))
      case Stop => Stop
    }

  private def outmapAux[K[-_, +_], I, O, P, A](p: Plan[K, I, O, A])(f: O => P): Plan[K, I, P, A] = p match {
    case r@Return(x)          => r
    case Emit(o, xs)          => Emit(f(o), () => outmapAux(xs())(f))
    case Await(k, succ, fail) => Await(k andThen (outmapAux(_)(f)), succ, () => outmapAux(fail())(f))
    case Stop                 => Stop
  }

  private def iomapAux[K[-_, +_], J, I, O, P, A](p: Plan[K, I, O, A])(f: J => I, g: O => P)(implicit K: Profunctor[K]): Plan[K, J, P, A] =
    p match {
      case r@Return(x)          => r
      case Emit(o, next)        => Emit(g(o), () => iomapAux(next())(f, g))
      case Await(k, succ, fail) => Await(k andThen (iomapAux(_)(f, g)), K.lmap(succ)(f), () => iomapAux(fail())(f, g))
      case Stop                 => Stop
    }

  private def foldMapAux[K[-_, +_], I, O, A, B](p: Plan[K, I, O, A])(f: O => B)(implicit M: Monoid[B]): B = p match {
    case Await(_, _, z) => foldMapAux(z())(f)
    case Emit(o, next)  => foldMapAux(next())(f)
    case _              => M.zero
  }

  private def foldRightAux[K[-_, +_], I, O, A, R](p: Plan[K, I, O, A])(z: => R, f: (O, => R) => R): R = p match {
    case Await(_, _, w) => foldRightAux(w())(z, f)
    case Emit(o, next)  => foldRightAux(next())(z, f)
    case _              => z
  }

  private def foldLeftAux[K[-_, +_], I, O, A, R](p: Plan[K, I, O, A])(acc: R, f: (R, O) => R): R = p match {
    case Await(_, _, w) => foldLeftAux(w())(acc, f)
    case Emit(o, next)  => foldLeftAux(next())(f(acc, o), f)
    case _              => acc
  }

  private def fittingAux[K[-_, +_], L[-_, +_], J, I, O, A](p: Plan[K, I, O, A],g: Fitting[K, L, J, I]): Plan[L, J, O, A] =
    p match {
      case Stop           => Stop
      case Emit(o, next)  => Emit(o, () => fittingAux(next(),g))
      case Await(k, s, f) => Await(k andThen (fittingAux(_,g)), g(s), () => fittingAux(f(),g))
      case r@Return(x)    => r
    }

  private def andThenAux[K[-_, +_], I, O, P, A](pl: Plan[K, I, O, A], p: Process[O, P]): Plan[K, I, P, A] = p match {
     case Emit(o, xs) => Emit(o, () => andThenAux(pl,xs()))
     case Stop => Stop
     case Await(k, s, f) => pl match {
       case r@Return(_) => r
       case Stop => andThenAux(Machine.stopped, f())
       case Emit(o, xs) => andThenAux(xs(), k(s(o)))
       case Await(l, t, g) => Await(l andThen (andThenAux(_, p)), t, () => andThenAux(g(), p))
     }
   }

  private def iterateAux[K[-_, +_], I, O, A](f: A => Plan[K, I, O, A])(p: Plan[K, I, O, A]): Machine[K, I, O] = p match {
    case Emit(o, xs) => Emit(o, () => iterateAux(f)(xs()))
    case Stop => Stop
    case Await(k, s, fail) => Await(k andThen (iterateAux(f)(_)), s, () => iterateAux(f)(fail()))
    case Return(a) => iterateAux(f)(f(a))
  }
}
