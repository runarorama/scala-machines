name := "machines"

version := "1.0"

description := "Streaming I/O for Scala"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.2"

libraryDependencies += "org.scalaz" %% "scalaz-effect" % "7.0.2"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"

scalaVersion := "2.10.2"

crossScalaVersions := Seq("2.9.2", "2.10.1", "2.10.2")

scalacOptions ++= Seq("-deprecation", "-unchecked")

scalacOptions <++= scalaVersion map {
  case sv if sv.contains("2.10") =>
    Seq("-feature", "-language:implicitConversions", "-language:higherKinds", "-language:existentials", "-language:postfixOps")
  case _ =>
    Seq("-Ydependent-method-types")
}

seq(bintraySettings:_*)

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

publishMavenStyle := true

