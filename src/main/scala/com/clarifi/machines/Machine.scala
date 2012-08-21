package com.clarifi.machines

import scalaz._
import scalaz.syntax.arrow._

object Machine {
  implicit def machineFunctor[K[-_, +_], I]: Functor[({type λ[+α] = Machine[K, I, α]})#λ] with
                                             Foldable[({type λ[+α] = Machine[K, I, α]})#λ] =
    new Functor[({type λ[+α] = Machine[K, I, α]})#λ] with
        Foldable[({type λ[+α] = Machine[K, I, α]})#λ] {
      def map[A, B](m: Machine[K, I, A])(f: A => B): Machine[K, I, B] = m outmap f
      def foldMap[A, B](m: Machine[K, I, A])(f: A => B)(implicit M: Monoid[B]) = m foldMap f
      def foldRight[A, B](m: Machine[K, I, A], z: => B)(f: (A, => B) => B): B = m.foldRight(z)(f)
    }

  implicit def machineProfunctor[K[-_, +_]](implicit K: Profunctor[K]):
        Profunctor[({type λ[-α, +β] = Machine[K, α, β]})#λ] =
    new Profunctor[({type λ[-α, +β] = Machine[K, α, β]})#λ] {
      def lmap[A, B, C](k: Machine[K, A, B])(f: C => A): Machine[K, C, B] = k inmap f
      def rmap[A, B, C](k: Machine[K, A, B])(f: B => C): Machine[K, A, C] = k outmap f
    }

  import Plan._

  implicit object ProcessCategory extends Category[Process] {
    def id[A]: Process[A, A] = (for {
      i <- await[A]
      o <- emit(i)
    } yield o).repeatedly

    def compose[A, B, C](m: Process[B, C], n: Process[A, B]): Process[A, C] =
      n andThen m
  }

  def pass[K[-_,+_], I, O](h: Handle[K, I, O]): Machine[K, I, O] =
    awaits(h) flatMap { x => emit(x) } repeatedly

  def stopped[K[-_,+_], I, O]: Machine[K, I, O] = Stop
}
