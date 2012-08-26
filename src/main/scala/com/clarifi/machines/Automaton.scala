// Copyright   :  (C) 2012 Rúnar Bjarnason, Paul Chiusano, Dan Doel, Edward Kmett
// License     :  BSD-style (see the file LICENSE)

package com.clarifi.machines

/**
 * Finite state automata.
 */
trait Automaton[-I, +O] {
  def process: Process[I, O]
}

import Plan._

/**
 * Mealy machines.
 */
case class Mealy[-I, +O](step: I => (O, Mealy[I, O])) extends Automaton[I, O] {
  def process: Process[I, O] = await flatMap { (i: I) =>
    step(i) match {
      case (o, next) => emit(o) >> next.process
    }
  }
}

/**
 * Moore machines.
 */
case class Moore[-I, +O](msg: O, next: I => Moore[I, O]) extends Automaton[I, O] {
  def process: Process[I, O] = emit(msg) >> (await flatMap { (i: I) => (next(i)).process })
}
