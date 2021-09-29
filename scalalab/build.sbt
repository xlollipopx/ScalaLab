name := "scalalab"

version := "0.1"

scalaVersion := "2.13.6"
val catsVersion        = "2.6.1"
val catsTaglessVersion = "0.14.0"
val catsEffectVersion  = "3.2.8"
val circeVersion       = "0.14.1"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core"           % catsVersion,
  "org.typelevel" %% "cats-effect"         % catsEffectVersion,
  "org.typelevel" %% "cats-tagless-macros" % catsTaglessVersion
)
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.9"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"
libraryDependencies += "eu.timepit"    %% "refined"   % "0.9.26"
libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)
