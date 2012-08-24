package com.clarifi.machines

import scalaz._
import scalaz.syntax.arrow._
import Scalaz._

object Machine {
  implicit def machineFunctor[K]:
      Functor[({type λ[+α] = Machine[K, α]})#λ] with
      Foldable[({type λ[+α] = Machine[K, α]})#λ] =
    new Functor[({type λ[+α] = Machine[K, α]})#λ] with
        Foldable[({type λ[+α] = Machine[K, α]})#λ] {
      def map[A, B](m: Machine[K, A])(f: A => B): Machine[K, B] = m outmap f
      def foldMap[A, B](m: Machine[K, A])(f: A => B)(implicit M: Monoid[B]) = m foldMap f
      def foldRight[A, B](m: Machine[K, A], z: => B)(f: (A, => B) => B): B = m.foldRight(z)(f)
    }

  import Plan._

  implicit object ProcessCategory extends Category[Process] {
    def id[A]: Process[A, A] = (await[A] flatMap emit) repeatedly

    def compose[A, B, C](m: Process[B, C], n: Process[A, B]): Process[A, C] =
      n andThen m
  }

  def pass[K, O](h: Handle[K, O]): Machine[K, O] =
    awaits(h) flatMap { x => emit(x) } repeatedly

  def stopped[K, O]: Machine[K, O] = Stop

  def flattened[K, I](h: Handle[K, List[I]]): Machine[K, I] =
    awaits(h) flatMap (is => traversePlan_(is)(emit)) repeatedly
}
