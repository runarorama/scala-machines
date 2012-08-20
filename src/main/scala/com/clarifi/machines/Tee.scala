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

  def tee[A, AA, B, BB, C](ma: Process[A, AA], mb: Process[B, BB], m: Tee[AA, BB, C]): Tee[A, B, C] =
    Machine(m.step match {
      case Stop => Stop
      case Yield(o, k) => Yield(o, () => tee(ma, mb, k()))
      case v@Expect(f, L(kf), ff) => ma.step match {
        case Stop => tee(stopped, mb, ff()).step
        case Yield(a, k) => tee(k(), mb, f(kf(a))).step
        case u@Expect(g, kg, fg) => Expect[T, Either[A, B], Tee[A, B, C], Tee[A, B, C]](
                                         x => x,
                                         L(a => tee(g(kg(a)), mb, Machine(v))),
                                         () => tee(fg(), mb, Machine(v)))
      }
      case v@Expect(f, R(kf), ff) => mb.step match {
        case Stop => tee(ma, stopped, ff()).step
        case Yield(b, k) => tee(ma, k(), f(kf(b))).step
        case u@Expect(g, kg, fg) => Expect[T, Either[A, B], Tee[A, B, C], Tee[A, B, C]](
                                         x => x,
                                         R((b: B) => tee(ma, g(kg(b)), Machine(v))),
                                         () => tee(ma, fg(), Machine(v)))
      }
    })

  def addL[A, B, C, D](p: Process[A, B], t: Tee[B, C, D]): Tee[A, C, D] =
    tee(p, id, t)

  def addR[A, B, C, D](p: Process[B, C], t: Tee[A, C, D]): Tee[A, B, D] =
    tee(id, p, t)

  /*def capL[A, B, C](s: Source[A], t: Tee[A, B, C]): Process[B, C] =
    cappedT(addL(s, t))*/


}
