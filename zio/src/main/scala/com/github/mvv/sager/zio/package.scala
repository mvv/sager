package com.github.mvv.sager

import _root_.zio.{Has, IO, Runtime, Tag, ULayer, URIO, URLayer, ZIO, ZLayer, ZManaged}
import _root_.zio.internal.Platform

package object zio {
  type Haz[A] = Field[A, A]
  type Env[A, B <: A] = Field[A, B]
  type FoundService[A, R <: Record] = Record.FoundValue[A, A, R]
  type FoundSubService[A, R <: Record] = Record.FoundSub[A, A, R]
  type SEnv = clock.Clock with console.Console with system.System with random.Random with blocking.Blocking

  object SEnv {
    object Services {
      val live: SEnv =
        Record.empty
          .add[clock.Clock.Service](clock.Clock.Service.live)
          .add[console.Console.Service](console.Console.Service.live)
          .add[system.System.Service](system.System.Service.live)
          .add[random.Random.Service](random.Random.Service.live)
          .add[blocking.Blocking.Service](blocking.Blocking.Service.live)
    }
    val any: URLayer[SEnv, SEnv] = ZLayer.requires[SEnv]
    val live: ULayer[SEnv] = ZLayer.succeedMany(Services.live)
  }

  trait SagerRuntime extends Runtime[SEnv] {
    override val environment: SEnv = SEnv.Services.live
    override val platform: Platform = Platform.default
  }

  trait SagerApp extends SagerRuntime {
    def run(args: List[String]): URIO[SEnv, Int]
    final def main(args: Array[String]): Unit =
      try sys.exit(
        unsafeRun(
          (for {
            fiber <- run(args.toList).fork
            _ <- IO.effectTotal(java.lang.Runtime.getRuntime.addShutdownHook(new Thread {
              override def run(): Unit = {
                val _ = unsafeRunSync(fiber.interrupt)
              }
            }))
            result <- fiber.join
            _ <- fiber.interrupt
          } yield result)
        )
      )
      catch { case _: SecurityException => }
  }

  implicit class SagerHazSyntax[R <: Record](val record: R) extends AnyVal {
    def has[A: Tag](implicit witness: R <:< Haz[A]): Has[A] = Has(record.get[A])
  }

  implicit class SagerHasSyntax[R <: Has[_]](val has: R) extends AnyVal {
    def haz[A: Tag](implicit witness: R <:< Has[A]): Haz[A] = Field[A](has.get[A])
  }

  implicit class SagerZioSyntax[R, E, A](val self: ZIO[R, E, A]) extends AnyVal {
    def asHaz(implicit tag: Tag[A]): ZIO[R, E, Haz[A]] = self.map(Field[A](_))
    def asEnv[B >: A: Tag]: ZIO[R, E, Env[B, A]] = self.map(Field[B](_))
    def toLayerHaz(implicit tag: Tag[A]): ZLayer[R, E, Haz[A]] = ZLayer.fromEffectHaz(self)
    def toLayerEnv[B >: A: Tag]: ZLayer[R, E, Env[B, A]] = ZLayer.fromEffectEnv[B](self)
  }

  implicit class SagerZManagedSyntax[R, E, A](val self: ZManaged[R, E, A]) extends AnyVal {
    def asHaz(implicit tag: Tag[A]): ZManaged[R, E, Haz[A]] = self.map(Field[A](_))
    def asEnv[B >: A: Tag]: ZManaged[R, E, Env[B, A]] = self.map(Field[B](_))
  }

  final class SagerZLayerSucceedEnvSyntax[A](val dummy: Unit) extends AnyVal {
    def apply[B <: A](a: => B)(implicit tag: Tag[A]): ULayer[Env[A, B]] = ZLayer.succeedMany(Field[A](a))
  }

  final class SagerZLayerFromFunctionEnvSyntax[A](val dummy: Unit) extends AnyVal {
    def apply[C, B <: A](f: C => B)(implicit tag: Tag[A]): URLayer[C, Env[A, B]] =
      ZLayer.fromFunctionMany((c: C) => Field[A](f(c)))
  }

  final class SagerZLayerFromFunctionEnvMSyntax[A](val dummy: Unit) extends AnyVal {
    def apply[E, C, B <: A](f: C => IO[E, B])(implicit tag: Tag[A]): ZLayer[C, E, Env[A, B]] =
      ZLayer.fromFunctionManyM((c: C) => f(c).asEnv[A])
  }

  final class SagerZLayerFromEffectEnvSyntax[A](val dummy: Unit) extends AnyVal {
    def apply[R, E, B <: A](zio: ZIO[R, E, B])(implicit tag: Tag[A]): ZLayer[R, E, Env[A, B]] =
      ZLayer.fromEffectMany(zio.asEnv[A])
  }

  final class SagerZLayerFromManagedEnvSyntax[A](val dummy: Unit) extends AnyVal {
    def apply[R, E, B <: A](managed: ZManaged[R, E, B])(implicit tag: Tag[A]): ZLayer[R, E, Env[A, B]] =
      ZLayer.fromManagedMany(managed.asEnv[A])
  }

  final class SagerZLayerFromServiceEnvSyntax[A](val dummy: Unit) extends AnyVal {
    def apply[B <: A, A0](f: A0 => B)(implicit tag0: Tag[A0], tag: Tag[A]): URLayer[Haz[A0], Env[A, B]] =
      ZLayer.fromFunctionEnv[A]((c: Haz[A0]) => f(c.get[A0]))
  }

  final class SagerZLayerFromServicesEnvSyntax[A](val dummy: Unit) extends AnyVal {
    def apply[B <: A, A0, A1](f: (A0, A1) => B)(
        implicit tag0: Tag[A0],
        tag1: Tag[A1],
        tag: Tag[A]): URLayer[Haz[A0] with Haz[A1], Env[A, B]] =
      ZLayer.fromFunctionEnv[A]((a: Haz[A0] with Haz[A1]) => f(a.get[A0], a.get[A1]))
  }

  final class SagerZLayerFromServiceEnvMSyntax[A](val dummy: Unit) extends AnyVal {
    def apply[E, B <: A, A0](f: A0 => IO[E, B])(implicit tag0: Tag[A0], tag: Tag[A]): ZLayer[Haz[A0], E, Env[A, B]] =
      ZLayer.fromFunctionEnvM[A]((c: Haz[A0]) => f(c.get[A0]))
  }

  final class SagerZLayerFromServicesEnvMSyntax[A](val dummy: Unit) extends AnyVal {
    def apply[E, B <: A, A0, A1](f: (A0, A1) => IO[E, B])(
        implicit tag0: Tag[A0],
        tag1: Tag[A1],
        tag: Tag[A]): ZLayer[Haz[A0] with Haz[A1], E, Env[A, B]] =
      ZLayer.fromFunctionEnvM[A]((a: Haz[A0] with Haz[A1]) => f(a.get[A0], a.get[A1]))
  }

  implicit class SagerZLayerCompanionSyntax(val underlying: ZLayer.type) extends AnyVal {
    def succeedHaz[A: Tag](a: => A): ULayer[Haz[A]] = ZLayer.succeedMany(Field[A](a))
    def succeedEnv[A]: SagerZLayerSucceedEnvSyntax[A] = new SagerZLayerSucceedEnvSyntax[A](())
    def fromFunctionHaz[A, B: Tag](f: A => B): URLayer[A, Haz[B]] =
      ZLayer.fromFunctionMany((a: A) => Field[B](f(a)))
    def fromFunctionEnv[A]: SagerZLayerFromFunctionEnvSyntax[A] = new SagerZLayerFromFunctionEnvSyntax[A](())
    def fromFunctionHazM[E, A, B: Tag](f: A => IO[E, B]): ZLayer[A, E, Haz[B]] =
      ZLayer.fromFunctionManyM((a: A) => f(a).asHaz)
    def fromFunctionEnvM[A]: SagerZLayerFromFunctionEnvMSyntax[A] = new SagerZLayerFromFunctionEnvMSyntax[A](())
    def fromEffectHaz[R, E, A: Tag](zio: ZIO[R, E, A]): ZLayer[R, E, Haz[A]] = ZLayer.fromEffectMany(zio.asHaz)
    def fromEffectEnv[A]: SagerZLayerFromEffectEnvSyntax[A] = new SagerZLayerFromEffectEnvSyntax[A](())
    def fromManagedHaz[R, E, A: Tag](managed: ZManaged[R, E, A]): ZLayer[R, E, Haz[A]] =
      ZLayer.fromManagedMany(managed.asHaz)
    def fromManagedEnv[A]: SagerZLayerFromManagedEnvSyntax[A] = new SagerZLayerFromManagedEnvSyntax[A](())
    def fromServiceHaz[A: Tag, B: Tag](f: A => B): URLayer[Haz[A], Haz[B]] =
      fromFunctionHaz((a: Haz[A]) => f(a.get[A]))
    def fromServiceEnv[A]: SagerZLayerFromServiceEnvSyntax[A] = new SagerZLayerFromServiceEnvSyntax[A](())
    def fromServicesHaz[A0: Tag, A1: Tag, A: Tag](f: (A0, A1) => A): URLayer[Haz[A0] with Haz[A1], Haz[A]] =
      fromFunctionHaz((a: Haz[A0] with Haz[A1]) => f(a.get[A0], a.get[A1]))
    def fromServicesEnv[A]: SagerZLayerFromServicesEnvSyntax[A] = new SagerZLayerFromServicesEnvSyntax[A](())
    def fromServiceHazM[E, A: Tag, B: Tag](f: A => IO[E, B]): ZLayer[Haz[A], E, Haz[B]] =
      fromFunctionHazM((a: Haz[A]) => f(a.get[A]))
    def fromServiceEnvM[A]: SagerZLayerFromServiceEnvMSyntax[A] = new SagerZLayerFromServiceEnvMSyntax[A](())
    def fromServicesHazM[E, A0: Tag, A1: Tag, A: Tag](
        f: (A0, A1) => IO[E, A]): ZLayer[Haz[A0] with Haz[A1], E, Haz[A]] =
      fromFunctionHazM((a: Haz[A0] with Haz[A1]) => f(a.get[A0], a.get[A1]))
    def fromServicesEnvM[A]: SagerZLayerFromServicesEnvMSyntax[A] = new SagerZLayerFromServicesEnvMSyntax[A](())
    def haz[A]: URLayer[Haz[A], Haz[A]] = ZLayer.requires[Haz[A]]
  }

  final class SagerZLayerUpdateEnvSyntax[A, RIn, E, ROut <: Record](val self: ZLayer[RIn, E, ROut]) extends AnyVal {
    def apply[B <: A, C <: A](f: B => C)(
        implicit tag: Tag[A],
        found: Record.FoundSub[A, B, ROut]): ZLayer[RIn, E, found.Rest with Env[A, C]] =
      self >>> ZLayer.fromFunctionMany(_.update(f))
  }

  implicit class SagerZLayerOutRSyntax[RIn, E, ROut <: Record](val self: ZLayer[RIn, E, ROut]) extends AnyVal {
    def updateHaz[A: Tag](f: A => A)(implicit found: Record.FoundValue[A, A, ROut]): ZLayer[RIn, E, ROut] =
      self >>> ZLayer.fromFunctionMany(_.updateMono(f))
    def updateEnv[A: Tag]: SagerZLayerUpdateEnvSyntax[A, RIn, E, ROut] =
      new SagerZLayerUpdateEnvSyntax[A, RIn, E, ROut](self)
  }
}
