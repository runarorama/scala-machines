package com.clarifi.machines

sealed trait These[+A, +B]
case class This[+A](a: A) extends These[A, Nothing]
case class That[+B](b: B) extends These[Nothing, B]
case class Both[+A, +B](a: A, b: B) extends These[A, B]
