package com.github.mvv.sager.zio

import java.io.IOException

import _root_.zio.{RIO, UIO, ULayer, URLayer, ZIO, ZLayer, blocking => zioBlocking}

package object blocking {
  type Blocking = Haz[zioBlocking.Blocking.Service]
  type BlockingEnv[A <: zioBlocking.Blocking.Service] = Env[zioBlocking.Blocking.Service, A]
  val Blocking: zioBlocking.Blocking.type = zioBlocking.Blocking

  object SagerBlocking {
    val any: URLayer[Blocking, Blocking] = ZLayer.requires[Blocking]
    val live: ULayer[Blocking] = ZLayer.succeedHaz(Blocking.Service.live)
  }

  def blocking[R <: Blocking, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
    ZIO.accessM[R](_.value.blocking(zio))
  def effectBlocking[A](effect: => A): RIO[Blocking, A] =
    RIO.accessM[Blocking](_.value.effectBlocking(effect))
  def effectBlockingCancelable[A](effect: => A)(cancel: UIO[Unit]): RIO[Blocking, A] =
    ZIO.accessM[Blocking](_.value.effectBlockingCancelable(effect)(cancel))
  def effectBlockingIO[A](effect: => A): ZIO[Blocking, IOException, A] =
    effectBlocking(effect).refineToOrDie[IOException]
  def effectBlockingInterrupt[A](effect: => A): RIO[Blocking, A] =
    RIO.accessM(_.value.effectBlockingInterrupt(effect))
}
