package com.clarifi.machines

import scalaz.Id._
import scalaz.std.list._

import org.scalacheck.{Properties, Prop}
import Prop._

import Plan._

object TestRepeatedly extends Properties("Tee") {
  property("repeatedly doesn't stack overflow given huge input") = secure {
    Process.transduce(List.range(0,10000000)) {
      await[Int] flatMap emit repeatedly
    }
    true
  }
}
