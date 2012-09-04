// Copyright   :  (C) 2012 Rúnar Bjarnason, Paul Chiusano, Dan Doel, Edward Kmett
// License     :  BSD-style (see the file LICENSE)

package com.clarifi.machines

import scalaz._
import scalaz.syntax.monoid._
import scalaz.syntax.monad._

/**
 * A `Procedure` is a `Machine` together with a driver for driving
 * that machine through the use of side-effects. A `Procedure[T]`
 * is conceptually a stream of elements of type `T`.
 */
trait Procedure[+A] { self =>
  type K

  def map[B](f: A => B): Procedure[B] =
    new Procedure[B] {
      type K = self.K
      def machine = self.machine.outmap(f)
      def withDriver[R](f: Driver[K] => R) = self withDriver f
    }

  def machine: Machine[K, A]

  def withDriver[R](f: Driver[K] => R): R

  def foldMap[R](f: A => R)(implicit R: Monoid[R]): R =
    withDriver(d => d.drive(machine)(f))

  def execute[B >: A](implicit B: Monoid[B]): B =
    foldMap[B](x => x)

  def andThen[B](p: Process[A, B]): Procedure[B] =
    new Procedure[B] {
      type K = self.K
      def machine = self.machine andThen p
      def withDriver[R](f: Driver[K] => R) = self withDriver f
    }

  def tee[B,C](p: Procedure[B], t: Tee[A, B, C]): Procedure[C] =
    new Procedure[C] {

      type K = self.K \/ p.K

      def machine = Tee.tee(self.machine, p.machine, t)

      def withDriver[R](k: Driver[K] => R): R =
        self.withDriver(d1 => p.withDriver(d2 => k(d1 * d2)))
    }

}

