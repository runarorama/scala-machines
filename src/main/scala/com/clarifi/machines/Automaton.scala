package com.clarifi.machines

trait Automaton[-I, +O] {
  def process: Process[I, O]
}

import Plan._

case class Mealy[-I, +O](step: I => (O, Mealy[I, O])) extends Automaton[I, O] {
  def process: Process[I, O] = await flatMap { (i: I) =>
    step(i) match {
      case (o, next) => emit(o) before next.process
    }
  }
}

case class Moore[-I, +O](msg: O, next: I => Moore[I, O]) extends Automaton[I, O] {
  def process: Process[I, O] = emit(msg) before (await flatMap { (i: I) => (next(i)).process })
}
