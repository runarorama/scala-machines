// SMRC begin clarifi
organization := "clarifi"

publishMavenStyle := false

version := { ("git rev-parse HEAD" !!).trim }

publishArtifact in (Compile, packageDoc) := false

publishArtifact in (Compile, packageSrc) := false
// SMRC end clarifi

name := "machines"

resolvers += "Scala Tools Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0-SNAPSHOT"

libraryDependencies += "org.scalaz" %% "scalaz-effect" % "7.0-SNAPSHOT"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.9" % "test"

scalaVersion := "2.9.2"

scalacOptions ++= Seq("-deprecation", "-Ydependent-method-types", "-unchecked")
