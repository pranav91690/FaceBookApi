
name := "FaceBook"

version := "1.0"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-feature", "-deprecation")

resolvers ++= Seq(
  "RoundEights" at "http://maven.spikemark.net/roundeights",
  "spray repo" at "http://repo.spray.io",
  "justwrote" at "http://repo.justwrote.it/releases/"
)

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.4.0",
  "com.typesafe.akka" %% "akka-remote" % "2.4.0",
  "com.typesafe.akka" %% "akka-testkit" % "2.4.0",
  "com.typesafe" % "config" % "1.3.0",
  "com.roundeights" %% "hasher" % "1.2.0",
  "io.spray" %% "spray-can" % "1.3.3",
  "io.spray" %% "spray-client" % "1.3.3",
  "io.spray" %% "spray-caching" % "1.3.3",
  "io.spray" %% "spray-routing" % "1.3.3",
  "io.spray" %% "spray-json" % "1.3.2",
  "io.spray" %% "spray-testkit" % "1.3.3" % "test",
  "org.specs2" %% "specs2-core" % "3.6.5" % "test",
  "org.log4s" %% "log4s" % "1.2.1",
  "org.json4s" % "json4s-jackson_2.10" % "3.1.0",
  "it.justwrote" %% "scala-faker" % "0.3"
)