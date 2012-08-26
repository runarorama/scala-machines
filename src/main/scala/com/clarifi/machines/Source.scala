package com.clarifi.machines

import scalaz._

import Plan._

/**
 * A `Source[O]` is a machine that emits values of type `O` and never requests
 * input.
 */
object Source {

  /** Repeatedly emits the given value. */
  def repeated[O](o: O): Source[O] = emit(o).repeatedly

  /** Repeatedly cycles through the values in the given `Foldable`. */
  def cycled[F[_], O](os: F[O])(implicit F: Foldable[F]): Source[O] =
    traversePlan_(os)(emit).repeatedly

  /** Emits the values in the given `Foldable` once and then stops. */
  def source[F[_], O](os: F[O])(implicit F: Foldable[F]): Source[O] =
    traversePlan_(os)(emit).compile
}

