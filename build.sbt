import sbt._
import Keys._
import xerial.sbt.Sonatype._

inThisBuild(
  Seq(
    organization := "com.github.mvv.sager",
    version := "0.1-M8", // next is M9
    homepage := Some(url("https://github.com/mvv/sager")),
    scmInfo := Some(ScmInfo(url("https://github.com/mvv/sager"), "scm:git@github.com:mvv/sager.git")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(id = "mvv",
                name = "Mikhail Vorozhtsov",
                email = "mikhail.vorozhtsov@gmail.com",
                url = url("https://github.com/mvv"))
    ),
    sonatypeProjectHosting := Some(GitHubHosting("mvv", "sager", "mikhail.vorozhtsov@gmail.com"))
  )
)

ThisBuild / publishTo := sonatypePublishToBundle.value
ThisBuild / publishMavenStyle := true

lazy val sonatypeBundleReleaseIfNotSnapshot: Command = Command.command("sonatypeBundleReleaseIfNotSnapshot") { state =>
  val extracted = Project.extract(state)
  if (extracted.get(isSnapshot)) {
    val log = extracted.get(sLog)
    log.info("Snapshot version, doing nothing")
    state
  } else {
    Command.process("sonatypeBundleRelease", state)
  }
}

inThisBuild(
  Seq(
    crossScalaVersions := Seq("2.13.2", "2.12.11"),
    scalaVersion := crossScalaVersions.value.head,
    scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked", "-Xfatal-warnings")
  )
)

def isPriorTo2_13(version: String): Boolean =
  CrossVersion.partialVersion(version) match {
    case Some((2, minor)) => minor < 13
    case _                => false
  }

val specs2Version = "4.9.4"
val specs2 = "org.specs2" %% "specs2-core" % specs2Version

lazy val sager = (project in file("."))
  .settings(
    skip in publish := true,
    sonatypeProfileName := "com.github.mvv",
    sonatypeSessionName := s"Sager_${version.value}",
    commands += sonatypeBundleReleaseIfNotSnapshot
  )
  .aggregate(core, zio)

lazy val core = (project in file("core"))
  .settings(
    name := "sager",
    description := "Generic records for Scala",
    scalacOptions ++= {
      if (isPriorTo2_13(scalaVersion.value)) {
        Nil
      } else {
        Seq("-Ymacro-annotations")
      }
    },
    libraryDependencies ++= Seq(
      "dev.zio" %% "izumi-reflect" % "1.0.0-M2",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided,
      specs2 % Test
    ),
    libraryDependencies ++= {
      if (isPriorTo2_13(scalaVersion.value)) {
        Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full))
      } else {
        Nil
      }
    }
  )

lazy val zio = (project in file("zio"))
  .settings(
    name := "sager-zio",
    description := "Generic records as ZIO environments",
    libraryDependencies ++= Seq("dev.zio" %% "zio" % "1.0.0-RC21-2" % Provided, specs2 % Test)
  )
  .dependsOn(core)
