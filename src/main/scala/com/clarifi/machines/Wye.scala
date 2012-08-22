package com.clarifi.machines

sealed trait Y[-I, -J] extends Covariant

case class W[-I, -O](f: I => O)               extends Y[I, Any] {
  type X >: O
}
case class X[-J, -O](f: J => O)               extends Y[Any, J] {
  type X >: O
}
case class Z[-I, -J, -O](f: Either[I, J] => O) extends Y[I, J] {
  type X >: O
}

object Wye {
  import Machine._
  import Process._

  def wye[A, AA, B, BB, O](pa: Process[A, AA], pb: Process[B, BB], y: Wye[AA, BB, O]): Wye[A, B, O] =
    y match {
      case Stop              => Stop
      case Emit(o, next)     => Emit(o, () => wye(pa, pb, next()))
      case Await(k, W(kl), f) =>
        pa match {
          case Stop           => wye(stopped, pb, f())
          case Emit(a, next)  => wye(next(), pb, k(kl(a)))
          case Await(l, t, g) =>
            Await(
              (paa: Process[A, AA]) => wye(paa, pb, y),
              W(a => l(t(a))),
              () => wye(g(), pb, y)
            )
        }
      case Await(k, X(kr), f) =>
        pb match {
          case Stop           => wye(pa, stopped, f())
          case Emit(b, next)  => wye(pa, next(), k(kr(b)))
          case Await(l, t, g) =>
            Await(
              (pbb: Process[B, BB]) => wye(pa, pbb, y),
              X(b => l(t(b))),
              () => wye(pa, g(), y)
            )
        }
      case Await(k, Z(ke), f) =>
        pa match {
          case Emit(a, next) => wye(next(), pb, k(ke(Left(a))))
          case Stop           => pb match {
            case Emit(b, next) => wye(stopped, next(), k(ke(Right(b))))
            case Stop           => wye(stopped, stopped, f())
            case Await(l, t, g) =>
              Await(
                (pbb: Process[B, BB]) => wye(stopped, pbb, y),
                X(b => l(t(b))),
                () => wye(stopped, g(), y)
              )
          }
          case Await(la, ta, ga) => pb match {
            case Emit(b, next) => wye(pa, next(), k(ke(Right(b))))
            case Stop           =>
              Await(
                (paa: Process[A,AA]) => wye(paa, stopped, y),
                W(a => la(ta(a))),
                () => wye(ga(), stopped, y)
              )
            case Await(lb, tb, gb) =>
              Await(
                (x: Wye[A, B, O]) => x,
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
