package com.clarifi.machines

import scalaz._

import Machine._

sealed trait T[-I, +O] {
  def map[B](f: O => B): T[I, B]
}

case class L[-I, +O](k: I => O) extends T[Either[I, Any], O] {
  def map[B](f: O => B): T[Either[I, Any], B] = L(k andThen f)
}

case class R[-I, +O](k: I => O) extends T[Either[Any, I], O] {
  def map[B](f: O => B): T[Either[Any, I], B] = R(k andThen f)
}

object T {
  implicit def tFunctor[I]: Functor[({type λ[α] = T[I, α]})#λ] = new Functor[({type λ[α] = T[I, α]})#λ] {
    def map[A,B](m: T[I, A])(f: A => B): T[I, B] = m map f
  }
}

object Tee {
  import ProcessCategory._

  import Plan._

  def tee[A, AA, B, BB, C](ma: Process[A, AA], mb: Process[B, BB], m: Tee[AA, BB, C]): Tee[A, B, C] =
    m match {
      case Stop => Stop
      case Emit(o, k) => Emit(o, () => tee(ma, mb, k()))
      case Await(f, L(kf), ff) => ma match {
        case Stop => tee(stopped, mb, ff())
        case Emit(a, k) => tee(k(), mb, f(kf(a)))
        case Await(g, kg, fg) => Await[T, Either[A, B], C, Nothing, Tee[A, B, C]](
                                         x => x,
                                         L(a => tee(g(kg(a)), mb, m)),
                                         () => tee(fg(), mb, m))
      }
      case Await(f, R(kf), ff) => mb match {
        case Stop => tee(ma, stopped, ff())
        case Emit(b, k) => tee(ma, k(), f(kf(b)))
        case Await(g, kg, fg) => Await[T, Either[A, B], C, Nothing, Tee[A, B, C]](
                                         x => x,
                                         R((b: B) => tee(ma, g(kg(b)), m)),
                                         () => tee(ma, fg(), m))
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

  def cappedT[A]: Fitting[T, Function1, A, Either[A, A]] =
    new Fitting[T, Function1, A, Either[A, A]] {
      def apply[R](t: T[Either[A, A], R]): A => R = t match {
        case L(f) => f
        case R(f) => f
      }
    }

  def left[A]: Handle[T, A, A] = new Fitting[Function1, T, A, A] {
    def apply[R](f: A => R) = L(f)
  }
  def right[A]: Handle[T, A, A] = new Fitting[Function1, T, A, A] {
    def apply[R](f: A => R) = R(f)
  }
}
