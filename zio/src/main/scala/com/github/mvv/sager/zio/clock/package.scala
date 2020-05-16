package com.github.mvv.sager.zio

import java.time.{DateTimeException, OffsetDateTime}
import java.util.concurrent.TimeUnit

import _root_.zio.{ULayer, URIO, URLayer, ZIO, ZLayer, clock => zioClock}
import _root_.zio.duration.Duration

package object clock {
  type Clock = Haz[zioClock.Clock.Service]
  type ClockEnv[A <: zioClock.Clock.Service] = Env[zioClock.Clock.Service, A]
  val Clock: zioClock.Clock.type = zioClock.Clock

  object SagerClock {
    val any: URLayer[Clock, Clock] = ZLayer.requires[Clock]
    val live: ULayer[Clock] = ZLayer.succeedHaz(Clock.Service.live)
  }

  def currentTime(unit: => TimeUnit): URIO[Clock, Long] =
    URIO.accessM(_.value.currentTime(unit))
  val currentDateTime: ZIO[Clock, DateTimeException, OffsetDateTime] =
    ZIO.accessM(_.value.currentDateTime)
  val nanoTime: URIO[Clock, Long] =
    ZIO.accessM(_.value.nanoTime)
  def sleep(duration: => Duration): URIO[Clock, Unit] =
    ZIO.accessM(_.value.sleep(duration))
}
