package com.clarifi

import scalaz._
import syntax.foldable._

package object machines {
  import Plan._

  implicit def unapplyK[TC[_[_]], K[+_], P[+_[+_], +_, +_], O, A0](
    implicit TC0: TC[({type λ[α] = P[K, O, α] })#λ]): Unapply[TC, P[K, O, A0]] {
    type M[X] = P[K, O, X]
    type A = A0
  } = new Unapply[TC, P[K, O, A0]] {
    type M[X] = P[K, O, X]
    type A = A0
    def TC = TC0
    def apply(ma: P[K, O, A0]) = ma
  }

  type Machine[+K[+_], +O] = Plan[K, O, Nothing]

  type Process[-I, +O] = Machine[({type λ[+α] = I => α})#λ, O]

  type Source[+O] = Machine[Nothing, Any, O]

  type Tee[-A, -B, +C] = Machine[({ type λ[+α] = T[A, B, α]})#λ, C]

  type Wye[-I, -J, +O] = Machine[({ type λ[+α] = Y[I, J, α]})#λ, O]

  def traversePlan_[F[_], K[+_], O, A](as: F[A])(f: A => Plan[K, O, Unit])(implicit F: Foldable[F]): Plan[K, O, Unit] =
    as.traverse_[({type λ[α] = Plan[K, O, α]})#λ](f)(planInstance[K, O])

}

