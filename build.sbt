name := "machines"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.0-M8"

libraryDependencies += "org.scalaz" %% "scalaz-effect" % "7.0.0-M8"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"

scalaVersion := "2.9.2"

crossScalaVersions := Seq("2.9.2", "2.10.1")

scalacOptions ++= Seq("-deprecation", "-unchecked")

scalacOptions <++= scalaVersion map {
  case sv if sv.contains("2.10") =>
    Seq("-feature", "-language:implicitConversions", "-language:higherKinds", "-language:existentials", "-language:postfixOps")
  case _ =>
    Seq("-Ydependent-method-types")
}
