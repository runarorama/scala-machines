package com.clarifi

import scalaz._
import syntax.foldable._

package object machines {
  import Plan._
//
//  implicit def unapplyK[TC[_[_]], K[+_], P[+_[+_], +_, +_], O, A0](
//    implicit TC0: TC[({type λ[α] = P[K, O, α] })#λ]): Unapply[TC, P[K, O, A0]] {
//    type M[X] = P[K, O, X]
//    type A = A0
//  } = new Unapply[TC, P[K, O, A0]] {
//    type M[X] = P[K, O, X]
//    type A = A0
//    def TC = TC0
//    def apply(ma: P[K, O, A0]) = ma
//  }

  type Machine[+K <: Covariant, +O] = Plan[K, O, Nothing]

  type Process[-I, +O] = Machine[S[I], O]

  type Source[+O] = Machine[Nothing, O]

  type Tee[-A, -B, +C] = Machine[T[A, B], C]

  type Wye[-I, -J, +O] = Machine[Y[I, J], O]

  def traversePlan_[F[_], K <: Covariant, O, A](as: F[A])(f: A => Plan[K, O, Unit])(implicit F: Foldable[F]): Plan[K, O, Unit] =
    as.traverse_[({type λ[α] = Plan[K, O, α]})#λ](f)(planInstance[K, O])

}

