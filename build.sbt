enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin)

name := "phenoscape-kb-ui"

version := "0.1.0"

organization := "org.phenoscape"

scalaVersion := "2.12.3"

requiresDOM in Test := true

libraryDependencies ++= Seq(
  "io.github.outwatch" %%% "outwatch" % "0.10.2",
  "org.scalatest" %%% "scalatest" % "3.0.3" % Test
)
