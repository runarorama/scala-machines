import CrossVersion.partialVersion

name := "machines"

version := "1.0"

description := "Streaming I/O for Scala"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.6"

libraryDependencies += "org.scalaz" %% "scalaz-effect" % "7.0.6"

libraryDependencies += {
  val scalacheck = if (scalaVersion.value == "2.9.2") "1.10.0" else "1.11.3"
  "org.scalacheck" %% "scalacheck" % scalacheck % "test"
}

scalaVersion := "2.11.0"

crossScalaVersions := Seq("2.9.2", "2.9.3", "2.10.3", "2.11.0")

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

