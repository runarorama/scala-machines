package com.clarifi.machines

import scalaz.Scalaz._

object Test {
  def main(args: Array[String]): Unit = {
    import Machine._
    import Plan._
    import Process._
    import Tee._
    import Source._

    val l1 = List(1 -> List(1,2,3), 3 -> List(3,4,5), 4 -> List(6,7,8,9))
    val l2 = List(1 -> List(0,1,2), 2 -> List(3,4,5), 4 -> List(1,2,3))

    println(cap(capL(source(l1),mergeOuter[Int,Int,Int]), source(l2)).foldMap(List(_)))
  }
}

