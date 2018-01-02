import sbt.url

organization := "io.github.definiti"

name := "scala-model"

version := "0.2.0-SNAPSHOT"

scalaVersion := "2.12.4"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "io.github.definiti" %% "core" % "0.2.0"
libraryDependencies += "commons-io" % "commons-io" % "2.6"
libraryDependencies += "com.typesafe" % "config" % "1.3.2"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2"
libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.25"
libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.4.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.5" % "test"
libraryDependencies += "com.47deg" %% "scalacheck-toolbox-datetime" % "0.2.3" % "test"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.8" % "test"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-language:implicitConversions", "-feature")

unmanagedSourceDirectories in Test += baseDirectory.value / "src" / "main" / "resources" / "native"

useGpg := true

pomIncludeRepository := { _ => false }

licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

homepage := Some(url("https://definiti.github.io"))

scmInfo := Some(
  ScmInfo(
    url("https://github.com/definiti/definiti-scala-model"),
    "scm:git@github.com:definiti/definiti-scala-model.git"
  )
)

developers := List(
  Developer(
    id = "kneelnrise",
    name = "Gaëtan Rizio",
    email = "gaetan@rizio.fr",
    url = url("https://github.com/kneelnrise")
  )
)

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}