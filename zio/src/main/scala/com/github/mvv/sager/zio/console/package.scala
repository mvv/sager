package com.github.mvv.sager.zio

import java.io.IOException

import _root_.zio.{ULayer, URLayer, ZIO, ZLayer, console => zioConsole}

package object console {
  type Console = Haz[zioConsole.Console.Service]
  type ConsoleEnv[A <: zioConsole.Console.Service] = Env[zioConsole.Console.Service, A]
  val Console: zioConsole.Console.type = zioConsole.Console

  object SagerConsole {
    val any: URLayer[Console, Console] = ZLayer.requires[Console]
    val live: ULayer[Console] = ZLayer.succeedHaz(Console.Service.live)
  }

  def putStr(line: => String): ZIO[Console, IOException, Unit] =
    ZIO.accessM(_.value.putStr(line))
  def putStrLn(line: => String): ZIO[Console, IOException, Unit] =
    ZIO.accessM(_.value.putStrLn(line))
  val getStrLn: ZIO[Console, IOException, String] =
    ZIO.accessM(_.value.getStrLn)
}
