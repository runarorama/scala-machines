package com.clarifi.machines

trait Automaton[-I, +O] {
  def process: Process[I, O]
}

import Plan._

case class Mealy[-I, +O](step: I => (O, Mealy[I, O])) extends Automaton[I, O] {
  def process = {
    def go(m: Mealy[I, O]): Plan[Function1, I, O, Nothing] =
      await flatMap { (i: I) =>
        m.step(i) match {
          case (o, next) => emit(o) >> go(next)
        }
      }
    go(this) compile
  }
}

case class Moore[-I, +O](msg: O, next: I => Moore[I, O]) extends Automaton[I, O] {
  def process = {
    def go(m: Moore[I, O]): Plan[Function1, I, O, Nothing] =
      emit(m.msg) >> (await flatMap { (i: I) => go(m next i) })
    go(this) compile
  }
}
