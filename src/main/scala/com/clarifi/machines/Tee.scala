package com.clarifi.machines

import scalaz._
import scalaz.syntax.order._
import Scalaz._
import Ordering._

import Machine._

object Tee {
  import ProcessCategory._
  import Plan._

  import scalaz.syntax.order._
  import scalaz.\/._
  import scalaz.std.vector._

  private def mergeOuterAux[A, B, K:Order](ka: K,
                                           ca: Vector[A],
                                           kb: K,
                                           cb: Vector[B]): Tee[(K, Vector[A]),
                                                               (K, Vector[B]),
                                                               These[A, B]] = {
    val o = Order[K].orderSyntax ; import o._
    ka ?|? kb match {
      case LT => traversePlan_(ca)(a =>
        emit(This(a))) >> awaits(left[(K, Vector[A])]) flatMap {
          case (kap, cap) => mergeOuterAux(kap, cap, kb, cb)
        } orElse flattened(right[Vector[B]]).
                   inmap(_.map(_.compose((p: (K, Vector[B])) => p._2))).
                   outmap(That(_))
      case GT => traversePlan_(cb)(b =>
        emit(That(b))) >> awaits(right[(K, Vector[B])]) flatMap {
          case (kbp, cbp) => mergeOuterAux(ka, ca, kbp, cbp)
        } orElse flattened(left[Vector[A]]).
                   inmap(_.bimap(_.compose((p: (K, Vector[A])) => p._2),
                                 x => x)).outmap(This(_))
      case EQ => traversePlan_(for {
          a <- ca
          b <- cb
        } yield Both(a, b))(emit _) >> mergeOuterChunks
    }
  }

  import Process._

  /** A merge outer join according to keys of type `K`. */
  def mergeOuterJoin[A, B, K:Order](f: A => K, g: B => K): Tee[A, B, These[A, B]] =
    tee(groupingBy(f), groupingBy(g))(mergeOuterChunks)

  /**
   * A merge outer join of chunks according to keys of type `K`.
   */
  def mergeOuterChunks[A, B, K:Order]: Tee[(K, Vector[A]),
                                           (K, Vector[B]),
                                           These[A, B]] =
    awaits(left[(K, Vector[A])]) flatMap {
      case (ka, as) => awaits(right[(K, Vector[B])]) flatMap {
        case (kb, bs) => mergeOuterAux(ka, as, kb, bs)
      } orElse flattened(left[Vector[A]]).
               inmap(_.bimap(_.compose((p: (K, Vector[A])) => p._2), x => x)).
               outmap(This(_))
    } orElse flattened(right[Vector[B]]).
             inmap(_.map(_.compose((p: (K, Vector[B])) => p._2))).
             outmap(That(_))

  /**
   * A natural hash join according to keys of type `K`.
   */
  def hashJoin[A, B, K](f: A => K, g: B => K): Tee[A, B, (A, B)] = {
    def build(m: Map[K, Vector[A]]): Plan[T[A, B], Nothing, Map[K, Vector[A]]] = (for {
      a  <- awaits(left[A])
      val ak = f(a)
      mp <- build(m.updated(ak, m.getOrElse(ak, Vector.empty) :+ a))
    } yield mp) orElse Return(m)
    for {
      m <- build(Map())
      r <- (awaits(right[B]) flatMap (b => {
        val k = g(b)
        m.getOrElse(k, Vector.empty)
         .foldr(Return(()): Plan[T[A, B], (A, B), Unit])(a => r => Emit(a -> b, () => r))
      })) repeatedly
    } yield r
  }

  /**
   * Feeds the output of two machines into a `Tee`. The result is a machine whose
   * inputs are described by the coproduct of the inputs of the two machines.
   */
  def tee[A, AA, B, BB, C](
    ma: Machine[A, AA],
    mb: Machine[B, BB]
  )(t: Tee[AA, BB, C]): Machine[A \/ B, C] = t match {
      case Stop => Stop
      case Emit(o, k) => Emit(o, () => tee(ma, mb)(k()))
      case Await(k, s, f) => s.fold(
        kl => ma match {
          case Stop => tee(ma, mb)(f())
          case Emit(a, next) => tee(next(), mb)(k(kl(a)))
          case Await(g, kg, fg) =>
            Await(g andThen ((m: Machine[A, AA]) => tee(m, mb)(t)),
                  \/.left(kg),
                  () => tee(fg(), mb)(t))
        },
        kr => mb match {
          case Stop => tee(ma, mb)(f())
          case Emit(b, next) => tee(ma, next())(k(kr(b)))
          case Await(g, kg, fg) =>
            Await(g andThen ((m: Machine[B, BB]) => tee(ma, m)(t)),
                  \/.right(kg),
                  () => tee(ma, fg())(t))
        }
      )
  }

  /** Combines two inputs into one. */
  def cappedT[A](t: T[A, A]): A => Any = t.fold(x => x, x => x)

  /** Request an input on the left. */
  def left[A]: Handle[T[A, Any], A] = \/.left(_)

  /** Request an input on the right. */
  def right[A]: Handle[T[Any, A], A] = \/.right(_)


}
