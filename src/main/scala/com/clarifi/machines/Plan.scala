// Copyright   :  (C) 2012 Rúnar Bjarnason, Dan Doel, Edward Kmett
// License     :  BSD-style (see the file LICENSE)

package com.clarifi.machines

import scalaz._

/**
 * You can construct a `Plan` and then `compile` it to a `Machine`.
 * A `Plan[K, O, A]` is a specification for a pure `Machine` that reads inputs selected by `K`,
 * writes values of type `O`, and has intermediate results of type `A`.
 */
trait Plan[+K[+_], +O, +A] {
  def flatMap[L[+Y] >: K[Y], P >: O, B](
    f: A => Plan[L, P, B]): Plan[L, P, B]
  def orElse[L[+Y] >: K[Y], P >: O, B >: A](
    o: => Plan[L, P, B]): Plan[L, P, B]

  def map[B](f: A => B): Plan[K, O, B] = flatMap(x => Return(f(x)))

  def iomap[L[+_], P](f: K ~> L, g: O => P): Plan[L, P, A] =
    Plan.iomapAux[L, O, P, A](this)(f, g)

  def outmap[P](f: O => P): Plan[K, P, A] = Plan.outmapAux(this)(f)

  def inmap[L[+_]](f: K ~> L): Plan[L, O, A] =
    Plan.inmapAux[L, O, A](this)(f)

  def foldMap[B](f: O => B)(implicit M: Monoid[B]): B = Plan.foldMapAux(this)(f)

  def foldRight[R](z: => R)(f: (O, => R) => R): R = Plan.foldRightAux(this)(z, f)

  def foldLeft[R](acc: R)(f: (R, O) => R): R = Plan. foldLeftAux(this)(acc, f)

  def >>[L[+Y] >: K[Y], P >: O, B](next: => Plan[L, P, B]): Plan[L, P, B] = flatMap { _ => next }

  def before[I, P >: O](next: => Process[I, P]): Process[I, P] = flatMap { _ => next }

  def compile: Machine[K, O] = >>(Stop)

  def repeatedly: Machine[K, O] = {
    lazy val r : Machine[K, O] = this >> r
    r
  }

  def iterate[L[+Y] >: K[Y], P >: O, B >: A](f: B => Plan[L, P, B]): Machine[L, P] =
    Plan.iterateAux(f)(this)

  def andThen[P](p: Process[O, P]): Plan[K, P, A] = Plan.andThenAux(this, p)
}

/** A pure `Plan`. */
case class Return[+A](a: A) extends Plan[Nothing, Nothing, A] {
  def flatMap[K[+Y] >: Nothing, O >: Nothing, B](
    f: A => Plan[K, O, B]): Plan[K, O, B] = f(a)
  def orElse[K[+Y] >: Nothing, O >: Nothing, B >: A](
    o: => Plan[K, O, B]): Plan[K, O, B] = this
}

/** Output a result. */
case class Emit[+K[+_], +O, +A](head: O, tail: () => Plan[K, O, A]) extends Plan[K, O, A] {
  def flatMap[L[+Y] >: K[Y], P >: O, B](
    f: A => Plan[L, P, B]): Plan[L, P, B] =
    Emit(head, () => tail() flatMap f)
  def orElse[L[+Y] >: K[Y], P >: O, B >: A](
    o: => Plan[L, P, B]): Plan[L, P, B] =
    Emit(head, () => tail() orElse o)
}

/** Wait for input. */
case class Await[+K[+_], +O, +A, Z](
  k: Z => Plan[K, O, A],
  success: K[Z],
  failure: () => Plan[K, O, A]) extends Plan[K, O, A] {
  def flatMap[L[+Y] >: K[Y], P >: O, B](
    f: A => Plan[L, P, B]): Plan[L, P, B] =
    Await(k andThen (_ flatMap f), success, () => failure() flatMap f)
  def orElse[L[+Y] >: K[Y], P >: O, B >: A](
    o: => Plan[L, P, B]): Plan[L, P, B] =
    Await((x: Z) => k(x) orElse o, success, () => o)
}

/** A plan that fails. */
case object Stop extends Plan[Nothing, Nothing, Nothing] {
  def flatMap[K[+Y] >: Nothing, O >: Nothing, B](
    f: Nothing => Plan[K, O, B]): Plan[K, O, B] = Stop
  def orElse[K[+Y] >: Nothing, O >: Nothing, A >: Nothing](
    o: => Plan[K, O, A]): Plan[K, O, A] = Stop
}

object Plan {
  implicit def planInstance[K[+_], O]: MonadPlus[({type λ[α] = Plan[K, O, α]})#λ] =
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
  def await[A]: Plan[({type λ[+α] = A => α})#λ, Nothing, A] = Await((a: A) => Return(a), x => x, fail)

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
  type Handle[+K[+_], +O] = ({type λ[α] = O => α})#λ ~> K

  /** Waits for input on a particular `Handle`. */
  def awaits[K[+_], O, J](f: Handle[K, J]): Plan[K, O, J] =
    Await((a: J) => Return(a), f(x => x), fail)

  // These `Aux` functions crash in 2.9.2 if they are defined directly on the class.
  // Related to a bug fixed in 2.10.0 regarding higher-kinded type checking
  private def inmapAux[K[+_], L[+_], O, A](p: Plan[K, O, A])(f: K ~> L): Plan[L, O, A] =
    p match {
      case Return(x) => Return(x)
      case Emit(o, xs) => Emit(o, () => inmapAux(xs())(f))
      case Await(k, succ, fail) => Await(k andThen (inmapAux(_)(f)), K.lmap(succ)(f), () => inmapAux(fail())(f))
      case Stop => Stop
    }

  private def outmapAux[K[+_], O, P, A](p: Plan[K, O, A])(f: O => P): Plan[K, P, A] = p match {
    case r@Return(x)          => r
    case Emit(o, xs)          => Emit(f(o), () => outmapAux(xs())(f))
    case Await(k, succ, fail) => Await(k andThen (outmapAux(_)(f)), succ, () => outmapAux(fail())(f))
    case Stop                 => Stop
  }

  private def iomapAux[K[+_], L[+_], O, P, A](p: Plan[K, O, A])(f: K ~> L, g: O => P): Plan[L, P, A] =
    p match {
      case r@Return(x)          => r
      case Emit(o, next)        => Emit(g(o), () => iomapAux(next())(f, g))
      case Await(k, succ, fail) => Await(k andThen (iomapAux(_)(f, g)), K.lmap(succ)(f), () => iomapAux(fail())(f, g))
      case Stop                 => Stop
    }

  private def foldMapAux[K[+_], O, A, B](p: Plan[K, O, A])(f: O => B)(implicit M: Monoid[B]): B = p match {
    case Await(_, _, z) => foldMapAux(z())(f)
    case Emit(o, next)  => foldMapAux(next())(f)
    case _              => M.zero
  }

  private def foldRightAux[K[+_], O, A, R](p: Plan[K, O, A])(z: => R, f: (O, => R) => R): R = p match {
    case Await(_, _, w) => foldRightAux(w())(z, f)
    case Emit(o, next)  => foldRightAux(next())(z, f)
    case _              => z
  }

  private def foldLeftAux[K[+_], O, A, R](p: Plan[K, O, A])(acc: R, f: (R, O) => R): R = p match {
    case Await(_, _, w) => foldLeftAux(w())(acc, f)
    case Emit(o, next)  => foldLeftAux(next())(f(acc, o), f)
    case _              => acc
  }

  private def andThenAux[K[+_], O, P, A](pl: Plan[K, O, A], p: Process[O, P]): Plan[K, P, A] = p match {
     case Emit(o, xs) => Emit(o, () => andThenAux(pl,xs()))
     case Stop => Stop
     case Await(k, s, f) => pl match {
       case r@Return(_) => r
       case Stop => andThenAux(Machine.stopped, f())
       case Emit(o, xs) => andThenAux(xs(), k(s(o)))
       case Await(l, t, g) => Await(l andThen (andThenAux(_, p)), t, () => andThenAux(g(), p))
     }
   }

  private def iterateAux[K[+_], O, A](f: A => Plan[K, O, A])(p: Plan[K, O, A]): Machine[K, O] = p match {
    case Emit(o, xs) => Emit(o, () => iterateAux(f)(xs()))
    case Stop => Stop
    case Await(k, s, fail) => Await(k andThen (iterateAux(f)(_)), s, () => iterateAux(f)(fail()))
    case Return(a) => iterateAux(f)(f(a))
  }
}
