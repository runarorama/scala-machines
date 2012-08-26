// Copyright   :  (C) 2012 Rúnar Bjarnason, Paul Chiusano, Dan Doel, Edward Kmett
// License     :  BSD-style (see the file LICENSE)

package com.clarifi.machines

import com.clarifi.machines._
import scalaz._
import scalaz.syntax.monoid._

/**
 * A `Procedure` is a `Machine` together with a routine for driving
 * that machine through the use of monadic effects.
 */
trait Procedure[M[_], A] {
  type K

  val machine: Machine[K, A]

  def withDriver[R](f: Driver[M, K] => M[R]): M[R]
}

