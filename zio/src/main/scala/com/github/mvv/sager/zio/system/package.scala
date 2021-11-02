package com.github.mvv.sager.zio

import _root_.zio.{RIO, ULayer, URIO, URLayer, ZIO, ZLayer, system => zioSystem}

package object system {
  type System = Haz[zioSystem.System.Service]
  type SystemEnv[A <: zioSystem.System.Service] = Env[zioSystem.System.Service, A]
  val System: zioSystem.System.type = zioSystem.System

  object SagerSystem {
    val any: URLayer[System, System] = ZLayer.requires[System]
    val live: ULayer[System] = ZLayer.succeedHaz(System.Service.live)
  }

  def env(variable: => String): ZIO[System, SecurityException, Option[String]] =
    ZIO.accessM(_.value.env(variable))
  def envOrElse(variable: String, alt: => String): ZIO[System, SecurityException, String] =
    ZIO.accessM(_.value.envOrElse(variable, alt))
  def envOrOption(variable: String, alt: => Option[String]): ZIO[System, SecurityException, Option[String]] =
    ZIO.accessM(_.value.envOrOption(variable, alt))
  val envs: ZIO[System, SecurityException, Map[String, String]] =
    ZIO.accessM(_.value.envs)
  val properties: RIO[System, Map[String, String]] =
    RIO.accessM(_.value.properties)
  def property(prop: => String): RIO[System, Option[String]] =
    RIO.accessM(_.value.property(prop))
  def propertyOrElse(prop: String, alt: => String): RIO[System, String] =
    RIO.accessM(_.value.propertyOrElse(prop, alt))
  def propertyOrOption(prop: String, alt: => Option[String]): RIO[System, Option[String]] =
    RIO.accessM(_.value.propertyOrOption(prop, alt))
  val lineSeparator: URIO[System, String] =
    URIO.accessM(_.value.lineSeparator)
}
