package com.clarifi.machines

sealed trait Y[-I, +O]

case class W[-I, +O](f: I => O)               extends Y[Either[I, Any], O]
case class X[-J, +O](f: J => O)               extends Y[Either[Any, J], O]
case class Z[-I, -J, O](f: Either[I, J] => O) extends Y[Either[I, J], O]

object Wye {
  import Machine._
  import Process._

  def wye[A, AA, B, BB, O](pa: Process[A, AA], pb: Process[B, BB], y: Wye[AA, BB, O]): Wye[A, B, O] =
    y match {
      case Stop               => Stop
      case Emit(o, next)     => Emit(o, () => wye(pa, pb, next()))
      case Await(k, W(s), f) => pa match {
        case Stop            => wye(stopped, pb, f())
        case Emit(a, next)  => wye(next(), pb, k(s(a)))
        case Await(l, t, g) =>
          Await[Y,Either[A,B],O,Nothing,Wye[A,B,O]](
            x => x,
            W(a => wye(l(t(a)), pb, y)),
            () => wye(g(), pb, y)
          )
      }
      case Await(k, X(s), f) => pb match {
        case Stop            => wye(pa, stopped, f())
        case Emit(b, next)  => wye(pa, next(), k(s(b)))
        case Await(l, t, g) =>
          Await[Y,Either[A,B],O,Nothing,Wye[A,B,O]](
            x => x,
            X(b => wye(pa, l(t(b)), y)),
            () => wye(pa, g(), y)
          )
      }
      case Await(k, Z(s), f) => pa match {
        case Emit(a, next) => wye(next(), pb, k(s(Left(a))))
        case Stop           => pb match {
          case Emit(b, next) => wye(stopped, next(), k(s(Right(b))))
          case Stop           => wye(stopped, stopped, f())
          case Await(l, t, g) =>
            Await[Y,Either[A,B],O,Nothing,Wye[A,B,O]](
              x => x,
              X(b => wye(stopped, l(t(b)), y)),
              () => wye(stopped, g(), y)
            )
        }
        case Await(la, ta, ga) => pb match {
          case Emit(b, next) => wye(pa, next(), k(s(Right(b))))
          case Stop           =>
            Await[Y,Either[A,B],O,Nothing,Wye[A,B,O]](
              x => x,
              W(a => wye(la(ta(a)), stopped, y)),
              () => wye(ga(), stopped, y)
            )
          case Await(lb, tb, gb) =>
            Await[Y,Either[A,B],O,Nothing,Wye[A,B,O]](
              x => x,
              Z {
                case Left(a)  => wye(la(ta(a)), pb, y)
                case Right(b) => wye(pa, lb(tb(b)), y)
              },
              () => wye(ga(), gb(), y)
            )
        }
      }
    }
}
