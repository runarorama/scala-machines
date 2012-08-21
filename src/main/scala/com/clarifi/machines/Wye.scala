package com.clarifi.machines

sealed trait Y[-I, +O]

case class W[-I, +O](f: I => O)               extends Y[Either[I, Any], O]
case class X[-J, +O](f: J => O)               extends Y[Either[Any, J], O]
case class Z[-I, -J, O](f: Either[I, J] => O) extends Y[Either[I, J], O]

object Wye {
  import Step._
  import Machine._
  import Process._

  def wye[A, AA, B, BB, O](pa: Process[A, AA], pb: Process[B, BB], y: Wye[AA, BB, O]): Wye[A, B, O] =
    Machine(y step match {
      case Stop               => Stop
      case Yield(o, next)     => Yield(o, () => wye(pa, pb, next()))
      case Expect(k, W(s), f) => pa step match {
        case Stop            => wye(stopped, pb, f()) step
        case Yield(a, next)  => wye(next(), pb, k(s(a))) step
        case Expect(l, t, g) =>
          Expect[Y,Either[A,B],Wye[A,B,O],Wye[A,B,O]](
            x => x,
            W(a => wye(l(t(a)), pb, y)),
            () => wye(g(), pb, y)
          )
      }
      case Expect(k, X(s), f) => pb step match {
        case Stop            => wye(pa, stopped, f()) step
        case Yield(b, next)  => wye(pa, next(), k(s(b))) step
        case Expect(l, t, g) =>
          Expect[Y,Either[A,B],Wye[A,B,O],Wye[A,B,O]](
            x => x,
            X(b => wye(pa, l(t(b)), y)),
            () => wye(pa, g(), y)
          )
      }
      case Expect(k, Z(s), f) => pa step match {
        case Yield(a, next) => wye(next(), pb, k(s(Left(a)))) step
        case Stop           => pb step match {
          case Yield(b, next) => wye(stopped, next(), k(s(Right(b)))) step
          case Stop           => wye(stopped, stopped, f()) step
          case Expect(l, t, g) =>
            Expect[Y,Either[A,B],Wye[A,B,O],Wye[A,B,O]](
              x => x,
              X(b => wye(stopped, l(t(b)), y)),
              () => wye(stopped, g(), y)
            )
        }
        case Expect(la, ta, ga) => pb step match {
          case Yield(b, next) => wye(pa, next(), k(s(Right(b)))) step
          case Stop           =>
            Expect[Y,Either[A,B],Wye[A,B,O],Wye[A,B,O]](
              x => x,
              W(a => wye(la(ta(a)), stopped, y)),
              () => wye(ga(), stopped, y)
            )
          case Expect(lb, tb, gb) =>
            Expect[Y,Either[A,B],Wye[A,B,O],Wye[A,B,O]](
              x => x,
              Z {
                case Left(a)  => wye(la(ta(a)), pb, y)
                case Right(b) => wye(pa, lb(tb(b)), y)
              },
              () => wye(ga(), gb(), y)
            )
        }
      }
    })
}
