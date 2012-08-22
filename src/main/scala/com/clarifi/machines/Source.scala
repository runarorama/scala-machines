package com.clarifi.machines

import scalaz._

import Plan._

object Source {
  def repeated[O](o: O): Source[O] = emit(o).repeatedly

  def cycled[F[_], O](os: F[O])(implicit F: Foldable[F]): Source[O] =
    traversePlan_(os)(emit).repeatedly

  def source[F[_], O](os: F[O])(implicit F: Foldable[F]): Source[O] =
    traversePlan_(os)(emit).compile
}

