import CrossVersion.partialVersion

name := "machines"

version := "1.0.1"

description := "Streaming I/O for Scala"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.6"

libraryDependencies += "org.scalaz" %% "scalaz-effect" % "7.2.6"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.2" % "test"

scalaVersion := "2.11.8"

crossScalaVersions := Seq("2.10.6", "2.11.8", "2.12.0-RC1")

scalacOptions ++= Seq("-deprecation", "-unchecked")

scalacOptions ++= {
  val non29 = Seq("-feature", "-language:implicitConversions", "-language:higherKinds", "-language:existentials", "-language:postfixOps")
  partialVersion(scalaVersion.value) match {
    case Some((2, 10)) => non29
    case sv => non29 ++ Seq("-Ywarn-unused-import")
  }
}

seq(bintraySettings:_*)

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

publishMavenStyle := true

