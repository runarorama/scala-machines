package com.clarifi.machines

import scalaz._
import scalaz.syntax.order._
import Scalaz._
import Ordering._

import Machine._

sealed trait TT[+F <: Covariant, +G <: Covariant] extends Covariant {
  def lmap[B <: Covariant](h: F => B): TT[B, G]
  def rmap[B <: Covariant](h: G => B): TT[F, B]

  def fold[R](fl: F => R, kr: G => R): R
}

case class TTL[+F <: Covariant](f: F) extends TT[F, Nothing] {
  type Ty = f.Ty

  def lmap[B <: Covariant](h: F => B) = TTL(h(f))
  def rmap[B <: Covariant](h: Nothing => B) = this

  def map[U](h: Ty => U) = TTL(f map h)

  def fold[R](kl: F => R, kr: Nothing => R) = kl(f)
}

case class TTR[+G <: Covariant](g: G) extends TT[Nothing, G] {
  type Ty = g.Ty

  def lmap[B <: Covariant](h: Nothing => B) = this
  def rmap[B <: Covariant](h: G => B) = TTR(h(g))

  def map[U](h: Ty => U) = TTR(g map h)

  def fold[R](kl: Nothing => R, kr: G => R) = kr(g)
}

sealed trait T[-I, -J] extends Covariant {
  def lmap[B](h: B => I): T[B, J]
  def rmap[B](h: B => J): T[I, B]

  def fold[R](kl: (I => Ty) => R, kr: (J => Ty) => R): R
}

case class L[-I, O](f: I => O) extends T[I, Any] {
  type Ty = O

  def lmap[B](h: B => I)   = L(f compose h)
  def rmap[B](h: B => Any) = this

  def map[U](h: Ty => U) = L(f andThen h)

  def fold[R](kl: (I => Ty) => R, kr: (Any => Ty) => R): R = kl(f)
}

case class R[-J, O](f: J => O) extends T[Any, J] {
  type Ty = O

  def lmap[B](h: B => Any) = this
  def rmap[B](h: B => J)   = R(f compose h)

  def map[U](h: Ty => U) = R(f andThen h)

  def fold[R](kl: (Any => Ty) => R, kr: (J => Ty) => R): R = kr(f)
}

object Tee {
  import ProcessCategory._
  import Plan._

  import scalaz.syntax.order._

  def mergeOuterAux[A, B, K:Order](ka: K, ca: List[A], kb: K, cb: List[B]): Tee[(K, List[A]), (K, List[B]), These[A, B]] = {
    val o = Order[K].orderSyntax ; import o._
    ka ?|? kb match {
      case LT => traversePlan_(ca)(a => emit(This(a))) >> awaits(left[(K, List[A])]) flatMap {
        case (kap, cap) => mergeOuterAux(kap, cap, kb, cb)
      } orElse flattened(right[List[B]]).inmap(_.rmap((p: (K, List[B])) => p._2)).outmap(That(_))
      case GT => traversePlan_(cb)(b => emit(That(b))) >> awaits(right[(K, List[B])]) flatMap {
        case (kbp, cbp) => mergeOuterAux(ka, ca, kbp, cbp)
      } orElse flattened(left[List[A]]).inmap(_.lmap((p: (K, List[A])) => p._2)).outmap(This(_))
      case EQ => traversePlan_(for { a <- ca ; b <- cb } yield Both(a, b))(emit _) >> mergeOuter
    }
  }

  def mergeOuter[A, B, K:Order]: Tee[(K, List[A]), (K, List[B]), These[A, B]] =
    awaits(left[(K, List[A])]) flatMap {
      case (ka, as) => awaits(right[(K, List[B])]) flatMap {
        case (kb, bs) => mergeOuterAux(ka, as, kb, bs)
      } orElse flattened(left[List[A]]).inmap(_.lmap((p: (K, List[A])) => p._2)).outmap(This(_))
    } orElse flattened(right[List[B]]).inmap(_.rmap((p: (K, List[B])) => p._2)).outmap(That(_))

  def tee[A, AA, B, BB, C](ma: Process[A, AA], mb: Process[B, BB], m: Tee[AA, BB, C]): Tee[A, B, C] = {
    m match {
      case Stop => Stop
      case Emit(o, k) => Emit(o, () => tee(ma, mb, k()))
      case Await(k, s, f) => s fold (
        kl => ma match {
          case Stop          => tee(ma, mb, f())
          case Emit(a, next) => tee(next(), mb, k(kl(a)))
          case Await(g, kg, fg) =>
            Await(
              (maa: Process[A, AA]) => tee(maa, mb, m),
              L(a => g(kg(a))),
              () => tee(fg(), mb, m)
            )
          },
        kr => mb match {
          case Stop         => tee(ma, mb, f())
          case Emit(b, next) => tee(ma, next(), k(kr(b)))
          case Await(g, kg, fg) =>
            Await(
              (mbb: Process[B, BB]) => tee(ma, mbb, m),
              R((b: B) => g(kg(b))),
              () => tee(ma, fg(), m)
            )
        }
      )
    }
  }

  def teeM[A <: Covariant, AA, B <: Covariant, BB, C](
    ma: Machine[A, AA],
    mb: Machine[B, BB],
    t: Tee[AA, BB, C]): Machine[TT[A, B], C] = t match {
      case Stop => Stop
      case Emit(o, k) => Emit(o, () => teeM(ma, mb, k()))
      case Await(k, s, f) => s.fold(
        kl => ma match {
          case Stop => teeM(ma, mb, f())
          case Emit(a, next) => teeM(next(), mb, k(kl(a)))
          case Await(g, kg, fg) =>
            Await(g andThen ((m: Machine[A, AA]) => teeM(m, mb, t)),
                  TTL(kg),
                  () => teeM(fg(), mb, t))
        },
        kr => mb match {
          case Stop => teeM(ma, mb, f())
          case Emit(b, next) => teeM(ma, next(), k(kr(b)))
          case Await(g, kg, fg) =>
            Await(g andThen ((m: Machine[B, BB]) => teeM(ma, m, t)),
                  TTR(kg),
                  () => teeM(ma, fg(), t))
        }
      )
  }

  def cappedT[A](t: T[A, A]): S[A] = t.fold(Fun(_), Fun(_))

  def left[A]: Handle[T[A, Any], A] =
    new Handle[T[A, Any], A] {
      def apply[R](f: A => R) = L(f)
    }

  def right[A]: Handle[T[Any, A], A] =
    new Handle[T[Any, A], A] {
      def apply[R](f: A => R) = R(f)
    }



}
