package com.clarifi.machines


import com.clarifi.machines._
import scalaz._
import scalaz.syntax.monoid._

trait Procedure[M[_], A] {
  type K

  val machine: Machine[K, A]

  def withDriver[R](f: Driver[M, K] => M[R]): M[R]
}

