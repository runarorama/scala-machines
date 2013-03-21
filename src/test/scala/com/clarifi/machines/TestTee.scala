package com.clarifi.machines

import scalaz.Id._
import scalaz.std.list._

import org.scalacheck.{Gen, Properties, Prop}
import Prop._
import com.clarifi.machines.Tee._
import com.clarifi.machines.Source._

object TestTee extends Properties("Tee") {
  val q1 = Source(List(("3", 1), ("5", 2), ("7", 2))).idProcedure
  val q2 = Source(List((2, "10"), (1, "9"), (12, "14"))).idProcedure

  property("hashjoin emits something, in order") = secure {
    val t = (q1 tee q2)(Tee.hashJoin(_._2, _._1))
    t.map(List(_)).execute ?= List((("5", 2), (2, "10")),
                                   (("7", 2), (2, "10")),
                                   (("3", 1), (1, "9")))
  }

  val randomPairs = Gen.listOf(
    for {
      n <- Gen.choose(0,10)
      m <- Gen.choose(0,10)
    } yield (n,m)) map (_.sortBy(_._1))

  property("merge outer join doesn't lose anything") = forAll(randomPairs, randomPairs) {
    (l1, l2) => {
      val results = mergeOuterJoin[(Int, Int), (Int, Int), Int](_._1, _._1).
        capL(source(l1)).cap(source(l2)).foldMap(List(_))
      val reference = (l1.map(_._1) ++ l2.map(_._1)).toSet

      results.map(t => t match {
        case Both(a, b) => a._1
        case This(a) => a._1
        case That(a) => a._1
      }).toSet.equals(reference)
    }
  }
}
