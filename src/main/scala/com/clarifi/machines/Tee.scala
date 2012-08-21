package com.clarifi.machines

import scalaz._

import Machine._

sealed trait T[-I, -J, +O] {
  def map[B](f: O => B): T[I, J, O]
  def lmap[B](f: B => I): T[B, J, O]
  def rmap[B](f: B => J): T[I, B, O]
}

case class L[-I, +O](k: I => O) extends T[I, Any, O] {
  def map[B](f: O => B): T[I, Any, B] = L(k andThen f)
  def lmap[B](f: B => I) = L(k compose f)
  def rmap[B](f: B => Any) = this
}

case class R[-I, +O](k: I => O) extends T[Any, I, O] {
  def map[B](f: O => B): T[Any, I, B] = R(k andThen f)
  def lmap[B](f: B => Any) = this
  def rmap[B](f: B => I) = R(k compose f)
}

object T {
  implicit def tFunctor[I, J]: Functor[({type λ[α] = T[I, J, α]})#λ] = new Functor[({type λ[α] = T[I, J, α]})#λ] {
    def map[A,B](m: T[I, J, A])(f: A => B): T[I, J, B] = m map f
  }
}

object Tee {
  import ProcessCategory._
  import Plan._

  import scalaz.syntax.order._

  /* def mergeOuter[A, B, K:Order]: Tee[(K, List[A]), (K, List[B]), These[A, B]] =
    awaits(left[(K, List[A])]) flatMap {
      case (k, as) => sys.error("sr")
    } orElse flattened(right[List[B]]).inmap(_._2) */

  def tee[A, AA, B, BB, C](ma: Process[A, AA], mb: Process[B, BB], m: Tee[AA, BB, C]): Tee[A, B, C] = {
    type Tab[+X] = T[A, B, X]
    m match {
      case Stop => Stop
      case Emit(o, k) => Emit(o, () => tee(ma, mb, k()))
      case Await(f, L(kf), ff) => ma match {
        case Stop => tee(stopped, mb, ff())
        case Emit(a, k) => tee(k(), mb, f(kf(a)))
        case Await(g, kg, fg) => Await[Tab, C, Nothing, Tee[A, B, C]](
                                         x => x,
                                         L(a => tee(g(kg(a)), mb, m)),
                                         () => tee(fg(), mb, m))
      }
      case Await(f, R(kf), ff) => mb match {
        case Stop => tee(ma, stopped, ff())
        case Emit(b, k) => tee(ma, k(), f(kf(b)))
        case Await(g, kg, fg) => Await[Tab, C, Nothing, Tee[A, B, C]](
                                         x => x,
                                         R((b: B) => tee(ma, g(kg(b)), m)),
                                         () => tee(ma, fg(), m))
      }
    }
  }

  def addL[A, B, C, D](p: Process[A, B], t: Tee[B, C, D]): Tee[A, C, D] =
    tee(p, id, t)

  def addR[A, B, C, D](p: Process[B, C], t: Tee[A, C, D]): Tee[A, B, D] =
    tee(id, p, t)

  def capL[A, B, C](s: Source[A], t: Tee[A, B, C]): Process[B, C] =
    addL(s, t) fitting cappedT

  def capR[A, B, C](s: Source[B], t: Tee[A, B, C]): Process[A, C] =
    addR(s, t) fitting cappedT

  def cappedT[A]: ({ type λ[+α] = T[A, A, α] })#λ ~> ({ type λ[+α] = A => α })#λ
    new (({ type λ[+α] = T[A, A, α] })#λ ~> ({ type λ[+α] = A => α })#λ) {
      def apply[R](t: T[A, A, R]): A => R = t match {
        case L(f) => f
        case R(f) => f
      }
    }

  def left[A]: Handle[({type λ[+α] = T[A, Any, α]})#λ, A] =
    new Handle[({type λ[+α] = T[A, Any, α]})#λ, A] {
      def apply[R](f: A => R) = L(f)
    }

  def right[A]: Handle[({type λ[+α] = T[Any, A, α]})#λ, A] =
    new Handle[({type λ[+α] = T[Any, A, α]})#λ, A] {
      def apply[R](f: A => R) = R(f)
    }
}
