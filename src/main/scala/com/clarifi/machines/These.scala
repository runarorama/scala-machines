package com.clarifi.machines

/**
 * `These[A,B]` has either an `A`, a `B`, or both.
 */
sealed trait These[+A, +B]
case class This[+A](a: A) extends These[A, Nothing]
case class That[+B](b: B) extends These[Nothing, B]
case class Both[+A, +B](a: A, b: B) extends These[A, B]
