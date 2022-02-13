import sbt._
import Keys._
import xerial.sbt.Sonatype._

inThisBuild(
  Seq(
    organization := "com.github.mvv.sager",
    version := "0.2-SNAPSHOT", // next is M1
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
    crossScalaVersions := Seq("3.0.2", "2.13.8"),
    scalaVersion := crossScalaVersions.value.head,
    scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked", "-Xfatal-warnings")
  )
)

def isScala2(version: String): Boolean =
  CrossVersion.partialVersion(version) match {
    case Some((2, _)) => true
    case _            => false
  }

inThisBuild(
  Seq(
    scalacOptions ++= {
      if (isScala2(scalaVersion.value)) {
        Seq("-Xsource:3.0")
      } else {
        Nil
      }
    }
  )
)

val scalatest = "org.scalatest" %% "scalatest" % "3.2.10"

lazy val sager = (project in file("."))
  .settings(
    publish / skip := true,
    sonatypeProfileName := "com.github.mvv",
    sonatypeSessionName := s"Sager_${version.value}",
    commands += sonatypeBundleReleaseIfNotSnapshot
  )
  .aggregate(core, zio, zioInteropCats)

lazy val core = (project in file("core"))
  .settings(
    name := "sager",
    description := "Generic records for Scala",
    Compile / scalaSource := {
      if (isScala2(scalaVersion.value)) {
        (Compile / scalaSource).value
      } else {
        baseDirectory.value / "src" / "main" / "scala3"
      }
    },
    Test / scalaSource := {
      if (isScala2(scalaVersion.value)) {
        (Test / scalaSource).value
      } else {
        baseDirectory.value / "src" / "test" / "scala3"
      }
    },
    scalacOptions ++= {
      if (isScala2(scalaVersion.value)) {
        Seq("-Ymacro-annotations")
      } else {
        Nil
      }
    },
    libraryDependencies ++= Seq(
      "com.github.mvv.typine" %% "typine" % "0.1-M4",
      "dev.zio" %% "izumi-reflect" % "1.1.3",
      scalatest % Test
    ),
    libraryDependencies ++= {
      if (isScala2(scalaVersion.value)) {
        Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided)
      } else {
        Nil
      }
    }
  )

lazy val zio = (project in file("zio"))
  .settings(
    name := "sager-zio",
    description := "Generic records as ZIO environments",
    libraryDependencies ++= Seq("dev.zio" %% "zio" % "1.0.12", scalatest % Test)
  )
  .dependsOn(core)

lazy val zioInteropCats = (project in file("zio-interop-cats"))
  .settings(
    name := "sager-zio-interop-cats",
    description := "Generic records as ZIO environments (Cats interop)",
    scalacOptions ++= {
      if (isScala2(scalaVersion.value)) {
        Nil
      } else {
        Seq("-Ykind-projector")
      }
    },
    libraryDependencies ++= {
      if (isScala2(scalaVersion.value)) {
        Seq(compilerPlugin(("org.typelevel" % "kind-projector" % "0.13.2").cross(CrossVersion.full)))
      } else {
        Nil
      }
    },
    libraryDependencies ++= Seq("dev.zio" %% "zio-interop-cats" % "3.1.1.0", scalatest % Test)
  )
  .dependsOn(zio)
