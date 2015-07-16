import CrossVersion.partialVersion

name := "machines"

version := "1.0.1"

description := "Streaming I/O for Scala"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.3"

libraryDependencies += "org.scalaz" %% "scalaz-effect" % "7.1.3"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.3" % "test"

scalaVersion := "2.11.7"

crossScalaVersions := Seq("2.9.3", "2.10.5", "2.11.7")

scalacOptions ++= Seq("-deprecation", "-unchecked")

scalacOptions ++= {
  val non29 = Seq("-feature", "-language:implicitConversions", "-language:higherKinds", "-language:existentials", "-language:postfixOps")
  partialVersion(scalaVersion.value) match {
    case Some((2, 9)) => Seq("-Ydependent-method-types")
    case Some((2, 10)) => non29
    case sv => non29 ++ Seq("-Ywarn-unused-import")
  }
}

seq(bintraySettings:_*)

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

publishMavenStyle := true

