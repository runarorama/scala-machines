package com.clarifi.machines

import scalaz.Scalaz._

object Test {
  def main(args: Array[String]): Unit = {
/*
    import Machine._
    import Plan._
    import Process._
*/
    import Tee._
    import Source._

    val l1 = List(1 -> 10, 2 -> 20, 3 -> 30)
    val l2 = List(1 -> 11, 3 -> 33, 4 -> 44)

    println(mergeOuterJoin[(Int, Int), (Int, Int), Int](_._1, _._1).
      capL(source(l1)).cap(source(l2)).foldMap(List(_)))
  }
}

