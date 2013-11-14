organization := "clarifi"

publishArtifact in (Compile, packageDoc) := false

publishArtifact in (Compile, packageSrc) := false

version := { ("git rev-parse HEAD" !!).trim }

publishMavenStyle := false


