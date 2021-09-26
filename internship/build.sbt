name := "internship"

version := "0.1"

scalaVersion := "2.13.3"

val http4sVersion           = "0.21.7"
val circeConfigVersion      = "0.8.0"
val doobieVersion           = "0.9.0"
val catsVersion             = "2.2.0"
val catsTaglessVersion      = "0.11"
val catsEffectVersion       = "2.2.0"
val epimetheusVersion       = "0.4.2"
val catsScalacheckVersion   = "0.2.0"
val log4CatsVersion         = "1.1.1"
val scalaTestVersion        = "3.1.0.0-RC2"
val h2Version               = "1.4.200"
val enumeratumVersion       = "1.6.1"
val dtoMapperChimneyVersion = "0.6.1"
val circeVersion            = "0.14.1"

libraryDependencies += "org.scalactic" %% "scalactic"   % "3.2.9"
libraryDependencies += "org.scalatest" %% "scalatest"   % "3.2.9" % "test"
libraryDependencies += "org.typelevel" %% "cats_effect" % "3.2.2"
libraryDependencies += "eu.timepit"    %% "refined"     % "0.9.15"
libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)
