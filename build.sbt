enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin)

name := "phenoscape-kb-ui"

version := "0.1.0"

organization := "org.phenoscape"

scalaVersion := "2.12.3"

scalacOptions ++= Seq("-deprecation", "-Ypartial-unification")

requiresDOM in Test := true

libraryDependencies ++= Seq(
  "io.github.outwatch"    %%% "outwatch"        % "0.10.2",
  "com.github.mariusmuja" %%% "outwatch-router" % "0.1.1-SNAPSHOT",
  "com.github.mariusmuja" %%% "outwatch-styles" % "0.1.1-SNAPSHOT",
  "com.github.mariusmuja" %%% "outwatch-mdl"    % "0.1.1-SNAPSHOT",
  "com.github.mariusmuja" %%% "outwatch-redux"  % "0.1.1-SNAPSHOT",
  "io.circe"              %%% "circe-core"      % "0.8.0",
  "io.circe"              %%% "circe-generic"   % "0.8.0",
  "io.circe"              %%% "circe-parser"    % "0.8.0",
  "org.scalatest"         %%% "scalatest"       % "3.0.3" % Test
)
