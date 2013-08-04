scalaVersion := "2.10.2"

organization := "info.folone"

name := "SoundCloud"

version := "0.1-SNAPSHOT"

// Scalaz
libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core"   % "7.1.0-M2",
  "org.scalaz" %% "scalaz-effect" % "7.1.0-M2"
)

scalacOptions += "-feature"
