package com.clarifi.machines

import scalaz._

/** The base functor for a `Machine`. */
trait Step[+K[-_, +_], -I, +O, +R] {
  def map[Q](f: R => Q): Step[K, I, O, Q] = this match {
    case Stop => Stop
    case Yield(o, k) => Yield(o, () => f(k()))
    case Expect(g, kg, fg) => Expect(f compose g, kg, () => f(fg()))
  }
}
case object Stop extends Step[Nothing, Any, Nothing, Nothing]
case class Yield[O, R](o: O, r: () => R) extends Step[Nothing, Any, O, R]
case class Expect[+K[-_, +_], -I, T, +R](k: T => R,
                                         s: K[I, T],
                                         r: () => R) extends Step[K, I, Nothing, R] {
  type E = T
}

object Step {
  implicit def stepFunctor[K[-_, +_], I, O]: Functor[({ type λ[α] = Step[K, I, O, α] })#λ] =
    new Functor[({ type λ[α] = Step[K, I, O, α] })#λ] {
      def map[A, B](m: Step[K, I, O, A])(f: A => B) = m map f
    }
}

