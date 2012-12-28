package com.clarifi.machines

import scalaz.Id._
import scalaz.std.list._

import org.scalacheck.{Properties, Prop}
import Prop._

object TestTee extends Properties("Tee") {
  val q1 = Source(List(("3", 1), ("5", 2), ("7", 2))).idProcedure
  val q2 = Source(List((2, "10"), (1, "9"), (12, "14"))).idProcedure

  property("hashjoin emits something, in order") = secure {
    val t = (q1 tee q2)(Tee.hashJoin(_._2, _._1))
    t.map(List(_)).execute ?= List((("5", 2), (2, "10")),
                                   (("7", 2), (2, "10")),
                                   (("3", 1), (1, "9")))
  }
}
