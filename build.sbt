name := "machines"

resolvers += "Scala Tools Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0-SNAPSHOT"

libraryDependencies += "org.scalaz" %% "scalaz-effect" % "7.0-SNAPSHOT"

scalaVersion := "2.9.2"

scalacOptions ++= Seq("-deprecation", "-Ydependent-method-types", "-unchecked")
