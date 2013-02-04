// Copyright   :  (C) 2012 Rúnar Bjarnason, Paul Chiusano, Dan Doel, Edward Kmett
// License     :  BSD-style (see the file LICENSE)

package com.clarifi.machines

import scalaz._
import scalaz.syntax.monoid._
import scalaz.syntax.monad._

/**
 * A `Procedure` is a `Machine` together with a driver for driving
 * that machine through the use of effects. A `Procedure[M, T]`
 * is conceptually a stream of elements of type `T` produced with effects described by `M`.
 */
trait Procedure[M[+_], +A] { self =>
  type K

  def map[B](f: A => B): Procedure[M, B] =
    new Procedure[M, B] {
      type K = self.K
      def machine = self.machine.outmap(f)
      def withDriver[R](f: Driver[M, K] => M[R]) = self withDriver f
    }

  def machine: Machine[K, A]

  def withDriver[R](f: Driver[M, K] => M[R]): M[R]

  def foldMapM[R](f: A => M[R])(implicit R: Monoid[R]): M[R] =
    withDriver(d => d.drive(machine)(f))

  def foreach(f: A => M[Unit]): M[Unit] = foldMapM(f)(Monoid instance ((_, b) => b, ()))

  def execute[B >: A](implicit B: Monoid[B]): M[B] =
    withDriver(d => d.drive(machine)(d.M.pure(_:B)))

  def foldLeftM[B >: A, C](initial: C)(f: (C, B) => C): M[C] =
    withDriver(d => d.driveLeft(machine)(d.M.pure(_:B))(initial)(f))

  def andThen[B](p: Process[A, B]): Procedure[M, B] =
    new Procedure[M, B] {
      type K = self.K
      def machine = self.machine andThen p
      def withDriver[R](f: Driver[M, K] => M[R]) = self withDriver f
    }

  def tee[B,C](p: Procedure[M, B])(t: Tee[A, B, C]): Procedure[M, C] =
    new Procedure[M, C] {

      type K = self.K \/ p.K

      def machine = Tee.tee(self.machine, p.machine)(t)

      def withDriver[R](k: Driver[M, K] => M[R]): M[R] =
        self.withDriver(d1 => p.withDriver(d2 => k(d1 * d2)))
    }

}
