name := "httpdsl"

version := "0.1"

scalaVersion := "2.12.4"

scalacOptions ++= Seq("-Ypartial-unification", "-Yrangepos", "-language:implicitConversions", "-language:higherKinds", "-deprecation")

val http4sVersion = "0.18.0-M4"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "1.0.0",
  "org.typelevel" %% "cats-core" % "1.0.0-RC1",
  "org.typelevel" %% "cats-free" % "1.0.0-RC1",
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
  "io.circe" %% "circe-generic" % "0.9.0-M1",
  "org.slf4j" % "slf4j-api" % "1.7.21",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.specs2" %% "specs2-core" % "4.0.1" % "test",
  "org.specs2" %% "specs2-scalacheck" % "4.0.1" % "test",
  "io.frees" %% "iota-core"  % "0.3.3"
)

crossPaths := false