package com.clarifi

import scalaz._
import syntax.foldable._

package object machines {
  import Plan._

  implicit def unapplyK[TC[_[_]], K[-_, +_], P[+_[-_, +_], -_, +_, +_], I, O, A0](
    implicit TC0: TC[({type λ[α] = P[K, I, O, α] })#λ]): Unapply[TC, P[K, I, O, A0]] {
    type M[X] = P[K, I, O, X]
    type A = A0
  } = new Unapply[TC, P[K, I, O, A0]] {
    type M[X] = P[K, I, O, X]
    type A = A0
    def TC = TC0
    def apply(ma: P[K, I, O, A0]) = ma
  }

  type Machine[+K[-_,+_], -I, +O] = Plan[K, I, O, Nothing]

  type Process[-I, +O] = Machine[Function1, I, O]

  type Source[+O] = Machine[Nothing, Any, O]

  type Tee[-A, -B, +C] = Machine[T, Either[A, B], C]

  type Wye[-I, -J, +O] = Machine[Y, Either[I, J], O]

  def traversePlan_[F[_], K[-_, +_], I, O, A](as: F[A])(f: A => Plan[K, I, O, Unit])(implicit F: Foldable[F]): Plan[K, I, O, Unit] =
    as.traverse_[({type λ[α] = Plan[K, I, O, α]})#λ](f)(planInstance[K, I, O])

}

