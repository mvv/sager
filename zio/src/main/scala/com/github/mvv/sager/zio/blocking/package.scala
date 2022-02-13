package com.github.mvv.sager.zio

import java.io.IOException
import _root_.zio.{blocking => zioBlocking, RIO, UIO, ULayer, URIO, URLayer, ZIO, ZLayer}
import _root_.zio.internal.Executor

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
  def blockingExecutor: URIO[Blocking, Executor] =
    URIO.access(_.value.blockingExecutor)
  def effectBlocking[A](effect: => A): RIO[Blocking, A] =
    RIO.accessM(_.value.effectBlocking(effect))
  def effectBlockingCancelable[A](effect: => A)(cancel: UIO[Unit]): RIO[Blocking, A] =
    RIO.accessM(_.value.effectBlockingCancelable(effect)(cancel))
  def effectBlockingInterrupt[A](effect: => A): RIO[Blocking, A] =
    RIO.accessM(_.value.effectBlockingInterrupt(effect))
  def effectBlockingIO[A](effect: => A): ZIO[Blocking, IOException, A] =
    effectBlocking(effect).refineToOrDie[IOException]
}
