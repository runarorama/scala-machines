package com.clarifi.machines

import scalaz.Id._
import scalaz.std.list._
import scalaz.std.stream._
import scalaz.syntax.foldable._

import org.scalacheck.{Properties, Prop}
import Prop._

import Plan._

object TestProcess extends Properties("Process") {
  property("transduce doesn't stack overflow given huge input") = secure {
    Process.transduce(List.range(0,10000000)) {
      await[Int] flatMap emit compile
    }
    true
  }
  property("repeated emits don't stack overflow given huge input") = secure {
    Process.transduce(List(0)) {
      traversePlan_(List.range(0,100000))(emit).compile
    }
    true
  }
  property("filtered") = secure {
    Process.transduce(Stream.from(1)) { Process.filtered[Int](_ % 2 == 0) andThen Process.taking(10000) }
    Process.transduce(Stream.from(1)) { Process.takingWhile(_ < 10000) }
    true
  }
}
