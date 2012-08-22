package com.clarifi.machines

sealed trait Y[-I, -J] extends Covariant {
  def contramap[A,B](g: A => I, h: B => J): Y[A, B]
  def lmap[B](h: B => I): Y[B, J] = contramap(h, x => x)
  def rmap[B](h: B => J): Y[I, B] = contramap(x => x, h)

  def fold[R](kr: (I => Ty) => R, kl: (J => Ty) => R, kb: (Either[I, J] => Ty) => R): R
}

case class W[-I, O](f: I => O)               extends Y[I, Any] {
  type Ty = O
  def contramap[A,B](g: A => I, h: B => Any) = W(f compose g)
  override def rmap[B](h: B => Any) = this
  def map[U](h: Ty => U)   = W(f andThen h)

  def fold[R](kr: (I => Ty) => R, kl: (Any => Ty) => R, ke: (Either[I, Any] => Ty) => R): R = kr(f)
}
case class X[-J, O](f: J => O)               extends Y[Any, J] {
  type Ty = O
  def contramap[A,B](g: A => Any, h: B => J) = X(f compose h)
  override def lmap[B](h: B => Any) = this
  def map[U](h: Ty => U)   = X(f andThen h)

  def fold[R](kr: (Any => Ty) => R, kl: (J => Ty) => R, ke: (Either[Any, J] => Ty) => R): R = kl(f)
}
case class Z[-I, -J, O](f: Either[I, J] => O) extends Y[I, J] {
  type Ty = O
  def contramap[A,B](g: A => I, h: B => J) = Z({
    case Left(a)  => f(Left(g(a)))
    case Right(b) => f(Right(h(b)))
  })
  def map[U](h: Ty => U) = Z(f andThen h)

  def fold[R](kr: (I => Ty) => R, kl: (J => Ty) => R, ke: (Either[I, J] => Ty) => R): R = ke(f)
}

object Wye {
  import Machine._
  import Process._

  def wye[A, AA, B, BB, O](pa: Process[A, AA], pb: Process[B, BB], y: Wye[AA, BB, O]): Wye[A, B, O] =
    y match {
      case Stop              => Stop
      case Emit(o, next)     => Emit(o, () => wye(pa, pb, next()))
      case Await(k, s, f) => s fold (
        kl => pa match {
          case Stop           => wye(stopped, pb, f())
          case Emit(a, next)  => wye(next(), pb, k(kl(a)))
          case Await(l, t, g) =>
            Await(
              (paa: Process[A, AA]) => wye(paa, pb, y),
              W(a => l(t(a))),
              () => wye(g(), pb, y)
            )
        },
        kr => pb match {
          case Stop           => wye(pa, stopped, f())
          case Emit(b, next)  => wye(pa, next(), k(kr(b)))
          case Await(l, t, g) =>
            Await(
              (pbb: Process[B, BB]) => wye(pa, pbb, y),
              X(b => l(t(b))),
              () => wye(pa, g(), y)
            )
        },
        ke => pa match {
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
      )
    }
}
