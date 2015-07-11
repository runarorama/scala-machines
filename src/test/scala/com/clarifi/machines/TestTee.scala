package com.clarifi.machines

import scalaz.std.list._

import org.scalacheck.{Gen, Properties, Prop}
import Prop._
import com.clarifi.machines.Tee._
import com.clarifi.machines.Source._
import scalaz.std.anyVal._

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
      k <- Gen.choose(0,10)
      v <- Gen.choose(0,10)
    } yield (k,v)) map (_.sortBy(_._1))
  // Gen.choose is buggy, sometimes emits negative numbers, but that's not a problem here

  property("merge outer join doesn't lose any keys") = forAll(randomPairs, randomPairs) {
    (l1, l2) => {
      val results = mergeOuterJoin[(Int, Int), (Int, Int), Int](_._1, _._1).
        capL(source(l1)).cap(source(l2)).foldMap(List(_))
      val reference = (l1.map(_._1) ++ l2.map(_._1)).toSet

      results.map(t => t match {
        case Both(a, b) => a._1
        case This(a) => a._1
        case That(b) => b._1
      }).toSet ?= reference
    }
  }

  property("merge outer join doesn't lose any values") = forAll(randomPairs, randomPairs) {
    (l1, l2) => {
      val rawResult = mergeOuterJoin[(Int, Int), (Int, Int), Int](_._1, _._1).
        capL(source(l1)).cap(source(l2)).foldMap(List(_))
      val result = rawResult.flatMap(_ match {
        case Both(a, b) => List(a, b)
        case This(a) => List(a)
        case That(b) => List(b)
      }).groupBy(_._1).mapValues(_.toSet)

      val reference = (l1 ++ l2).groupBy(_._1).mapValues(_.sorted.toSet)

      result ?= reference
    }
  }

  property("hash join works") = forAll(randomPairs, randomPairs) {
    (l1, l2) => {
      import scalaz._
      import Scalaz._

      val results = hashJoin[(Int, Int), (Int, Int), Int](_._1, _._1).
        capL(source(l1)).cap(source(l2)).foldMap(List(_)).map({
        case ((k, v1), (_, v2)) => (k, (v1, v2))
      }).sorted

      def cartesian[A](a: Seq[A], b: Seq[A]) = {
        for {
          aa <- a
          bb <- b
        } yield (aa, bb)
      }

      val reference = l1.groupBy(_._1).intersectWith(l2.groupBy(_._1)) {
        (v1, v2) => cartesian(v1, v2)
      }.toList.flatMap(_ match {
        case (k, pairs) => pairs.map(v => (k, (v._1._2, v._2._2)))
      }).sorted

      results ?= reference
    }
  }
}
