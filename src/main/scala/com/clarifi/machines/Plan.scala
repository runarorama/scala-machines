package com.clarifi.machines

import scalaz._

/**
 * You can construct a `Plan`, turning it into a `Machine`.
 */
trait Plan[+K[-_, +_], -I, +O, +A]
case class Return[+A](a: A) extends Plan[Nothing, Any, Nothing, A]
case class Emit[+K[-_, +_], -I, +O, +A](head: O, tail: () => Plan[K, I, O, A]) extends Plan[K, I, O, A]
case class Await[+K[-_, +_], -I, +O, +A, Z](
  k: Z => Plan[K, I, O, A],
  success: K[I, Z],
  failure: () => Plan[K, I, O, A]) extends Plan[K, I, O, A]
case object Fail extends Plan[Nothing, Any, Nothing, Nothing]

object Plan {
  implicit def planInstance[K[-_, +_], I, O, A]: MonadPlus[({type λ[+α] = Plan[K, I, O, α]})#λ] =
    new MonadPlus[({type λ[+α] = Plan[K, I, O, α]})#λ] {
      def bind[A, B](m: Plan[K, I, O, A])(f: A => Plan[K, I, O, B]) = m match {
        case Return(a) => f(a)
        case Emit(head, tail) => Emit(head, () => bind(tail())(f))
        case Await(k, s, fail) => Await(k andThen (bind(_)(f)), s, () => bind(fail())(f))
        case Fail => Fail
      }
      def point[A](a: => A) = Return(a)
      def empty[A] = Fail
      def plus[A](m1: Plan[K, I, O, A], m2: => Plan[K, I, O, A]) = m1 match {
        case Await(k, s, _) => Await(k andThen (plus(_, m2)), s, () => m2)
        case Emit(head, tail) => Emit(head, () => plus(tail(), m2))
        case Fail => m2
        case x => x
      }
    }

  def emit[A](a: A): Plan[Nothing, Any, A, Unit] = Emit(a, () => Return(()))
  def await[A]: Plan[Function1, A, Nothing, A] = Await((a: A) => Return(a), x => x, () => Fail)
}
