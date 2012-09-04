package com.clarifi.machines

import scalaz.{Reader => _, _}
import Scalaz._

import java.io._

object Example {

  import Machine.ProcessCategory._
  import Plan._

  def getFileLines[A](f: File, m: Process[String, A]): Procedure[A] =
    new Procedure[A] {
      type K = String => Any

      val machine = m

      def withDriver[R](k: Driver[K] => R): R = {
        val r = bufferFile(f)
        val d = new Driver[String => Any] {
          def apply(k: String => Any): Option[Any] = Option(r.readLine) map k
        }
        val result = k(d)
        r.close
        result
      }
    }

  def bufferFile(f: File): BufferedReader =
    new BufferedReader(new FileReader(f))

  def lineCount(fileName: String) =
    getFileLines(new File(fileName), Process(_ => 1)).execute

  def lineCharCount(fileName: String) =
    getFileLines(new File(fileName), Process(x => (1, x.length))).execute

  val words: Process[String, String] = (for {
    s <- await[String]
    _ <- traversePlan_(s.split("\\W").toList)(emit)
  } yield ()) repeatedly

  def lineWordCount(fileName: String) =
    getFileLines(new File(fileName),
      (id split words) outmap (_.fold(_ => (1, 0), _ => (0, 1)))) execute

}

