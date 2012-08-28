// Copyright   :  (C) 2012 Rúnar Bjarnason, Paul Chiusano,
//                         Dan Doel, Edward Kmett
// License     :  BSD-style (see the file LICENSE)

package com.clarifi.machines

import scalaz._

/**
 * You can construct a `Plan` and then `compile` it to a `Machine`.
 * A `Plan[K, O, A]` is a specification for a pure `Machine` that reads
 * inputs selected by `K` and writes values of type `O`. The `Plan` has
 * intermediate results of type `A` which are placeholders for further plans.
 */
sealed trait Plan[+K, +O, +A] {

  /**
   * Substitute into the variables in this `Plan`.
   */
  def flatMap[L >: K, P >: O, B](f: A => Plan[L, P, B]): Plan[L, P, B]

  /**
   * If the current `Plan` fails, try the plan `p`.
   */
  def orElse[L >: K, P >: O, B >: A](p: => Plan[L, P, B]): Plan[L, P, B]

  /**
   * Reassign the variables in this `Plan`.
   */
  def map[B](f: A => B): Plan[K, O, B] = flatMap { x => Return(f(x)) }

  /** Transform the inputs of the resulting machine. */
  def inmap[L](p: K => L): Plan[L, O, A] = this match {
    case r@Return(_)    => r
    case Stop           => Stop
    case Emit(o, next)  => Emit(o, () => next() inmap p)
    case Await(k, s, f) => Await(k andThen (_ inmap p), p(s), () => f() inmap p)
  }

  /** Transform the outputs of the resulting machine. */
  def outmap[P](f: O => P): Plan[K, P, A] = this match {
    case r@Return(x) => r
    case Emit(o, next) => Emit(f(o), () => next() outmap f)
    case Await(k, success, failure) =>
      Await(k andThen (_ outmap f), success, () => failure() outmap f)
    case Stop => Stop
  }

  /** Accumulate the outputs of the machine in a `Monoid`. */
  def foldMap[B](f: O => B)(implicit M: Monoid[B]): B = this match {
    case Emit(o, next)        => M.append(f(o), next() foldMap f)
    case Await(_, _, failure) => failure() foldMap f
    case _                    => M.zero
  }

  /** Accumulate the outputs of the machine, associating to the right. */
  def foldRight[R](z: => R)(f: (O, => R) => R): R = this match {
    case Emit(o, next)        => f(o, next().foldRight(z)(f))
    case Await(_, _, failure) => failure().foldRight(z)(f)
    case _                    => z
  }

  /** Accumulate the outputs of the machine, associating to the left. */
  def foldLeft[R](acc: R)(f: (R, O) => R): R = this match {
    case Emit(o, next)        => next().foldLeft(f(acc, o))(f)
    case Await(_, _, failure) => failure().foldLeft(acc)(f)
    case _                    => acc
  }

  /** Sequential composition of `Plan`s. */
  def >>[L >: K, P >: O, B](next: => Plan[L, P, B]): Plan[L, P, B] =
    flatMap { _ => next }

  /** A `Machine` that executes the `Plan` and then stops. */
  def compile: Machine[K, O] = >>(Stop)

  /** A `Machine` that executes the `Plan` indefinitely. */
  def repeatedly: Machine[K, O] = {
    lazy val r : Machine[K, O] = this >> r
    r
  }

  /** Repeat this plan `n` times. */
  def replicateM_(n: Int): Plan[K, O, Unit] = n match {
    case 0 => Return(())
    case n => this >> this.replicateM_(n - 1)
  }

  /** Corecursively build a machine by iterating the given function. */
  def iterate[L >: K, P >: O, B >: A](h: B => Plan[L, P, B]): Machine[L, P] =
    this match {
      case Return(x)      => h(x) iterate h
      case Stop           => Stop
      case Emit(o, next)  => Emit(o, () => next() iterate h)
      case Await(k, s, f) => Await(k andThen (_ iterate h), s, () => f() iterate h)
    }

  /** Connect the output of the machine to the given `Process`. */
  def andThen[P](p: Process[O, P]): Plan[K, P, A] = p match {
    case Emit(o, next)  => Emit(o, () => this andThen next())
    case Stop           => Stop
    case Await(k, s, f) => this match {
      case Stop           => Stop andThen f()
      case Emit(o, next)  => next() andThen k(s(o))
      case Await(l, t, g) => Await(l andThen (_ andThen p), t, () => g() andThen p)
      case r@Return(_)    => r
    }
  }

  /** Split the output into two streams at possibly differing rates. */
  def split[P](y: Process[O, P]): Plan[K, O \/ P, A] = (this, y) match {
    case (Emit(o, h), Emit(p, k)) =>
      Emit(\/.left(o), () => Emit(\/.right(p), () => h() split k()))
    case (Emit(o, h), Await(k, s, f)) => Emit(\/.left(o), () => h() split k(s(o)))
    case (Emit(o, h), _) => Emit(\/.left(o), () => h() split y)
    case (_, Emit(p, k)) => Emit(\/.right(p), () => this split k())
    case (Await(kl, sl, fl), _) =>
      Await(kl andThen (_ split y), sl, () => fl() split y)
    case (Stop, _) => Stop
    case (r@Return(_), _) => r
  }

  def sink[P, Q >: O](m: Moore[Q, P]): Moore[Q, P] = this match {
    case Emit(o, k) => k() sink m.next(o)
    case Await(_, _, f) => f() sink m
    case _ => m
  }

}

/**
 * A placeholder in the `Plan`, representing a variable of type `A` to be
 * assigned later.
 */
case class Return[+A](x: A) extends Plan[Nothing, Nothing, A] {
  def flatMap[L >: Nothing, P >: Nothing, B](f: A => Plan[L, P, B])   = f(x)
  def orElse[L >: Nothing, P >: Nothing, B >: A](p: => Plan[L, P, B]) = this
}

/** Emit an output value of type `O` and proceed with `Plan` `next`. */
case class Emit[+K, +O, +A](o: O, next: () => Plan[K, O, A]) extends Plan[K, O, A] {
  def flatMap[L >: K, P >: O, B](f: A => Plan[L, P, B]) =
    Emit(o, () => next() flatMap f)
  def orElse[L >: K, P >: O, B >: A](p: => Plan[L, P, B]) =
    Emit(o, () => next() orElse p)
}

/**
 * Await an input according to type `K`. Handle the input with continuation `k`
 * or continue with `failure` if receiving the input failed.
 */
case class Await[+K, +O, +A](k: Any => Plan[K, O, A],
                             success: K,
                             failure: () => Plan[K, O, A]) extends Plan[K, O, A] {
  def flatMap[L >: K, P >: O, B](f: A => Plan[L, P, B]) =
    Await(k andThen (_ flatMap f), success, () => failure() flatMap f)
  def orElse[L >: K, P >: O, B >: A](p: => Plan[L, P, B]) =
    Await(k andThen (_ orElse p), success, () => p)
}

/** Terminates the plan with a machine that accepts no input and emits no output. */
case object Stop extends Plan[Nothing, Nothing, Nothing] {
  def flatMap[L >: Nothing, P >: Nothing, B](f: Nothing => Plan[L, P, B]) = this
  def orElse[L >: Nothing, P >: Nothing, B >: Nothing](p: => Plan[L, P, B]) = this
}

object Plan {
  /** `Plan` is a ringad. */
  implicit def planInstance[K, O]: MonadPlus[({type λ[α] = Plan[K, O, α]})#λ] =
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
  def await[A]: Plan[A => Any, Nothing, A] = Await((a: Any) => Return(a.asInstanceOf[A]), x => x, fail)

  /** Waits for input on a particular `Handle`. */
  def awaits[K, J, O](f: Handle[K, J]): Plan[K, O, J] =
    Await((a: Any) => Return(a.asInstanceOf[J]), f(x => x), fail)
}
