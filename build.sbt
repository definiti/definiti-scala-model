organization := "definiti"

name := "scala-model"

version := "0.0.0"

scalaVersion := "2.12.1"

libraryDependencies += "definiti" %%  "core" % "0.1.0"
libraryDependencies += "commons-io" % "commons-io" % "2.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-language:implicitConversions", "-feature")