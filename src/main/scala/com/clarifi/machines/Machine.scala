package com.clarifi.machines

import scalaz._
import scalaz.syntax.arrow._

sealed trait Profunctor[K[-_, +_]] {
  def lmap[A, B, C](k: K[A, B])(f: C => A): K[C, B]
  def rmap[A, B, C](k: K[A, B])(f: B => C): K[A, C]
}

object Profunctor {
  implicit object FnProfunctor extends Profunctor[Function1] {
    type K[A, B] = A => B
    def lmap[A, B, C](k: K[A, B])(f: C => A): K[C, B] = k compose f
    def rmap[A, B, C](k: K[A, B])(f: B => C): K[A, C] = k andThen f
  }
}

sealed trait Machine[+K[-_, +_], -I, +O] {
  def step: Step[K, I, O, Machine[K, I, O]]

  def map[P](f: O => P): Machine[K, I, P] = Machine(step match {
    case Yield(o, xs) => Yield(f(o), () => xs() map f)
    case n@Expect(k, kir, e) => Expect((x: n.type#E) => k(x) map f, kir, () => e() map f)
    case Stop => Stop
  })

  def foldMap[B](f: O => B)(implicit M: Monoid[B]): B = step match {
    case Yield(o, xs) => M.append(f(o), xs() foldMap f)
    case Stop => M.zero
    case Expect(_, _, fg) => fg() foldMap f
  }

  def foldRight[B](z: B)(f: (O, => B) => B): B = step match {
    case Yield(o, xs) => f(o, xs().foldRight(z)(f))
    case Stop => z
    case Expect(_, _, fg) => fg().foldRight(z)(f)
  }

  def foldLeft[B](z: B)(f: => (B, O) => B): B = {
    def loop(m: Machine[K, I, O], b: B): B = m.step match {
      case Yield(o, xs) => loop(xs(), f(b, o))
      case Stop => b
      case Expect(_, _, fg) => fg().foldLeft(b)(f)
    }
    loop(this, z)
  }
}

object Machine {
  def apply[K[-_, +_], I, O](f: => Step[K, I, O, Machine[K, I, O]]): Machine[K, I, O] =
    new Machine[K, I, O] {
      lazy val step = f
    }

  def unapply[K[-_, +_], I, O](m: Machine[K, I, O]): Option[Step[K, I, O, Machine[K, I, O]]] =
    Some(m.step)

  implicit def machineFunctor[K[-_, +_], I]: Functor[({type λ[+α] = Machine[K, I, α]})#λ] with
                                             Foldable[({type λ[+α] = Machine[K, I, α]})#λ] =
    new Functor[({type λ[+α] = Machine[K, I, α]})#λ] with
        Foldable[({type λ[+α] = Machine[K, I, α]})#λ] {
      def map[A, B](m: Machine[K, I, A])(f: A => B): Machine[K, I, B] = m map f
      def foldMap[A, B](m: Machine[K, I, A])(f: A => B)(implicit M: Monoid[B]) = m foldMap f
      def foldRight[A, B](m: Machine[K, I, A], z: => B)(f: (A, => B) => B): B = m.foldRight(z)(f)
    }

  implicit def machineProfunctor[K[-_, +_]](implicit K: Profunctor[K]):
        Profunctor[({type λ[-α, +β] = Machine[K, α, β]})#λ] =
    new Profunctor[({type λ[-α, +β] = Machine[K, α, β]})#λ] {
      def lmap[A, B, C](k: Machine[K, A, B])(f: C => A): Machine[K, C, B] =
        Machine(k.step match {
          case Yield(o, xs) =>
            Yield(o, () => lmap(xs())(f))
          case t@Expect(k, kir, e) => Expect((x:t.type#E) =>
            lmap(k(x))(f), K.lmap(kir)(f), () => lmap(e())(f))
          case Stop => Stop
        })
      def rmap[A, B, C](k: Machine[K, A, B])(f: B => C): Machine[K, A, C] = k map f
    }

//  import Plan._

//  def pass[K[-_,+_], I, O](h: Handle[K, I, O]): Machine[K, I, O] =
//    awaits(h) flatMap { x => emit(x) } repeatedly

//  def stopped[K[-_,+_], I, O]: Machine[K, I, O] = Machine(Stop)
}
