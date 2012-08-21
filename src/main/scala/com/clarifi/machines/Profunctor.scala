package com.clarifi.machines

trait Profunctor[K[-_, +_]] {
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

