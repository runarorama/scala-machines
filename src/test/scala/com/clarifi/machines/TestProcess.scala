package com.clarifi.machines

import scalaz.Id._
import scalaz.std.list._
import scalaz.std.stream._
import scalaz.syntax.foldable._

import org.scalacheck.{Properties, Prop, Gen}
import Prop._

import Plan._
import scalaz.UnitReducer

object TestProcess extends Properties("Process") {
  property("transduce doesn't stack overflow given huge input") = secure {
    Process.transduce(List.range(0,10000000)) {
      await[Int] flatMap emit compile
    }
    true
  }
  property("repeated emits don't stack overflow given huge input") = secure {
    // TODO: this test takes ~120s on scala 2.10.0 (2.10.1 works fine) !!!
    Process.transduce(List(0)) {
      traversePlan_(List.range(0,100000))(emit).compile
    }
    true
  }
  property("filtered is lazy in its input") = secure {
    Process.transduce(Stream.from(1)) { Process.filtered[Int](_ % 2 == 0) andThen Process.taking(10000) }
    Process.transduce(Stream.from(1)) { Process.takingWhile(_ < 10000) }
    true
  }

  val randomInts = Gen.listOfN(10, Gen.choose(0, 10))

  property("reducer") = forAll(randomInts) {
    (l) => {
      def reducer(o: Int) = List.fill(o)(('a'.toInt + o).toChar)

      val results = Process.transduce(l)(Process.reducer(UnitReducer((i: Int) => reducer(i)))).flatten.toList
      results ?= l.flatMap(reducer(_))
    }
  }
  property("apply") = forAll(randomInts) {
    (l) => {
      Process.transduce(l)(Process((i: Int) => i + 10)).toList ?= l.map(_ + 10)
    }
  }
  property("buffered") = forAll(randomInts) {
    (l) => {
      val results = Process.transduce(l)(Process.buffered(2)).toList
      results.map(_.toList).filterNot(_.isEmpty) ?= l.grouped(2).toList
    }
  }
  property("droppingWhile") = forAll(randomInts, Gen.choose(0, 10)) {
    (l, d) => {
      Process.transduce(l)(Process.droppingWhile(_ < d)).toList ?= l.dropWhile(_ < d)
    }
  }
  property("dropping") = forAll(randomInts, Gen.choose(0, 10)) {
    (l, d) => {
      Process.transduce(l)(Process.dropping(d)).toList ?= l.drop(d)
    }
  }
  property("wrapping") = forAll(randomInts) {
    (l) => {
      Process.transduce(l)(Process.wrapping).toList ?= (l match {
        case Nil => List()
        case l => List(Vector() ++ l)
      })
    }
  }
  property("prepended") = forAll(randomInts, randomInts) {
    (l1, l2) => {
      Process.transduce(l1)(Process.prepended(l2)).toList ?= (l2 ++ l1)
    }
  }
}
