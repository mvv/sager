package com.github.mvv.sager.zio.interop

import _root_.zio.interop.{
  CatsEffectZManagedInstances,
  CatsMtlPlatform,
  CatsPlatform,
  CatsZManagedInstances,
  CatsZManagedSyntax,
  CatsZioInstances
}
import cats.data.State
import cats.{MonadError, Show, StackSafeMonad}
import cats.effect.kernel.{
  Async,
  Concurrent,
  Cont,
  Deferred,
  GenConcurrent,
  GenTemporal,
  Outcome,
  Poll,
  Sync,
  Temporal,
  Unique,
  Fiber => CFiber,
  Ref => CRef
}
import cats.effect.{LiftIO, IO => CIO}
import cats.effect.unsafe.IORuntime
import com.github.mvv.sager.zio.blocking.{effectBlocking, effectBlockingInterrupt, Blocking}
import com.github.mvv.sager.zio.clock.{currentTime, nanoTime, Clock}
import com.github.mvv.sager.zio.console.Console
import com.github.mvv.sager.zio.{clock, console, SEnv, SagerApp, SagerRuntime}
import zio.duration.Duration
import zio.{Cause, ERef, Exit, Fiber, FiberFailure, IO, Promise, RIO, Runtime, Task, ZIO, ZRef}

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS, NANOSECONDS}
import scala.concurrent.{ExecutionContext, Future}

package object catz extends SagerCatsEffectPlatform {
  object core extends CatsPlatform
  object mtl extends CatsMtlPlatform

  object implicits {
    implicit val rts: Runtime[Clock with Blocking] = SagerRuntime.default
  }
}

abstract class SagerCatsEffectInstances extends CatsZioInstances {
  implicit final def liftIOInstance[R](implicit runtime: IORuntime): LiftIO[RIO[R, *]] =
    new ZioLiftIO

  implicit final def asyncInstance[R <: Clock with Blocking]: Async[RIO[R, *]] =
    asyncInstance0.asInstanceOf[Async[RIO[R, *]]]

  implicit final def temporalInstance[R <: Clock, E]: GenTemporal[ZIO[R, E, *], E] =
    temporalInstance0.asInstanceOf[GenTemporal[ZIO[R, E, *], E]]

  implicit final def concurrentInstance[R, E]: GenConcurrent[ZIO[R, E, *], E] =
    concurrentInstance0.asInstanceOf[GenConcurrent[ZIO[R, E, *], E]]

  implicit final def asyncRuntimeInstance[E](implicit runtime: Runtime[Clock with Blocking]): Async[Task] =
    new ZioRuntimeAsync

  implicit final def temporalRuntimeInstance[E](implicit runtime: Runtime[Clock]): GenTemporal[IO[E, *], E] =
    new ZioRuntimeTemporal[E]

  private[this] val asyncInstance0: Async[RIO[Clock with Blocking, *]] =
    new ZioAsync

  private[this] val temporalInstance0: Temporal[RIO[Clock, *]] =
    new ZioTemporal

  private[this] val concurrentInstance0: Concurrent[Task] =
    new ZioConcurrent[Any, Throwable]
}

object SagerCatsConsole {
  def putStr[A](a: A)(implicit ev: Show[A]): ZIO[Console, Nothing, Unit] =
    console.putStr(ev.show(a)).orDie
  def putStrLn[A](a: A)(implicit ev: Show[A]): ZIO[Console, Nothing, Unit] =
    console.putStrLn(ev.show(a)).orDie
}

abstract class SagerCatsEffectPlatform
    extends SagerCatsEffectInstances
    with CatsEffectZManagedInstances
    with CatsZManagedInstances
    with CatsZManagedSyntax {

  trait CatsApp extends SagerApp {
    implicit val runtime: Runtime[SEnv] = this
  }

  val console: SagerCatsConsole.type = SagerCatsConsole
}

private class ZioLiftIO[R](implicit runtime: IORuntime) extends LiftIO[RIO[R, *]] {
  final override def liftIO[A](ioa: CIO[A]): RIO[R, A] =
    ZIO.effectAsync(k => ioa.unsafeRunAsync(k.compose(ZIO.fromEither(_))))
}

private class ZioMonadError[R, E] extends MonadError[ZIO[R, E, *], E] with StackSafeMonad[ZIO[R, E, *]] {
  type F[A] = ZIO[R, E, A]

  final override def pure[A](a: A): F[A] =
    ZIO.succeed(a)

  final override def map[A, B](fa: F[A])(f: A => B): F[B] =
    fa.map(f)

  final override def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
    fa.flatMap(f)

  final override def flatTap[A, B](fa: F[A])(f: A => F[B]): F[A] =
    fa.tap(f)

  final override def widen[A, B >: A](fa: F[A]): F[B] =
    fa

  final override def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
    fa.zipWith(fb)(f)

  final override def as[A, B](fa: F[A], b: B): F[B] =
    fa.as(b)

  final override def whenA[A](cond: Boolean)(f: => F[A]): F[Unit] =
    ZIO.when(cond)(f)

  final override def unit: F[Unit] =
    ZIO.unit

  final override def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A] =
    fa.catchAll(f)

  final override def recoverWith[A](fa: F[A])(pf: PartialFunction[E, F[A]]): F[A] =
    fa.catchSome(pf)

  final override def raiseError[A](e: E): F[A] =
    ZIO.fail(e)

  final override def attempt[A](fa: F[A]): F[Either[E, A]] =
    fa.either

  final override def adaptError[A](fa: F[A])(pf: PartialFunction[E, E]): F[A] =
    fa.mapError(pf.orElse { case error => error })
}

final private class ZioRef[R, E, A](ref: ERef[E, A]) extends CRef[ZIO[R, E, *], A] {
  type F[T] = ZIO[R, E, T]

  override def access: F[(A, A => F[Boolean])] =
    get.map { current =>
      val called = new AtomicBoolean(false)
      def setter(a: A): F[Boolean] =
        ZIO.effectSuspendTotal {
          if (called.getAndSet(true)) {
            ZIO.succeed(false)
          } else {
            ref.modify { updated =>
              if (current == updated) (true, a)
              else (false, updated)
            }
          }
        }

      (current, setter)
    }

  override def tryUpdate(f: A => A): F[Boolean] =
    update(f).as(true)

  override def tryModify[B](f: A => (A, B)): F[Option[B]] =
    modify(f).asSome

  override def update(f: A => A): F[Unit] =
    ref.update(f)

  override def modify[B](f: A => (A, B)): F[B] =
    ref.modify(f(_).swap)

  override def tryModifyState[B](state: State[A, B]): F[Option[B]] =
    modifyState(state).asSome

  override def modifyState[B](state: State[A, B]): F[B] =
    modify(state.run(_).value)

  override def set(a: A): F[Unit] =
    ref.set(a)

  override def get: F[A] =
    ref.get
}

final private class ZioDeferred[R, E, A](promise: Promise[E, A]) extends Deferred[ZIO[R, E, *], A] {
  type F[T] = ZIO[R, E, T]

  override val get: F[A] =
    promise.await

  override def complete(a: A): F[Boolean] =
    promise.succeed(a)

  override val tryGet: F[Option[A]] =
    promise.isDone.flatMap {
      case true  => get.asSome
      case false => ZIO.none
    }
}

private class ZioConcurrent[R, E] extends ZioMonadError[R, E] with GenConcurrent[ZIO[R, E, *], E] {
  import ZioConcurrent._

  private def toPoll(restore: ZIO.InterruptStatusRestore) =
    new Poll[ZIO[R, E, *]] {
      override def apply[T](fa: ZIO[R, E, T]): ZIO[R, E, T] = restore(fa)
    }

  private def toFiber[A](fiber: Fiber[E, A]) =
    new CFiber[F, E, A] {
      final override val cancel: F[Unit] = fiber.interrupt.unit
      final override val join: F[Outcome[F, E, A]] = fiber.await.map(toOutcome)
    }

  private def fiberFailure(error: E) =
    FiberFailure(Cause.fail(error))

  override def ref[A](a: A): F[CRef[F, A]] =
    ZRef.make(a).map(new ZioRef(_))

  override def deferred[A]: F[Deferred[F, A]] =
    Promise.make[E, A].map(new ZioDeferred(_))

  final override def start[A](fa: F[A]): F[CFiber[F, E, A]] =
    fa.interruptible.forkDaemon.map(toFiber)

  override def never[A]: F[A] =
    ZIO.never

  final override def cede: F[Unit] =
    ZIO.yieldNow

  final override def forceR[A, B](fa: F[A])(fb: F[B]): F[B] =
    fa.foldCauseM(cause => if (cause.interrupted) ZIO.halt(cause) else fb, _ => fb)

  final override def uncancelable[A](body: Poll[F] => F[A]): F[A] =
    ZIO.uninterruptibleMask(body.compose(toPoll))

  final override def canceled: F[Unit] =
    ZIO.interrupt

  final override def onCancel[A](fa: F[A], fin: F[Unit]): F[A] =
    fa.onError(cause => fin.orDieWith(fiberFailure).unless(cause.failed))

  final override def memoize[A](fa: F[A]): F[F[A]] =
    fa.memoize

  final override def racePair[A, B](fa: F[A], fb: F[B]) =
    (fa.interruptible raceWith fb.interruptible)(
      (exit, fiber) => ZIO.succeed(Left((toOutcome(exit), toFiber(fiber)))),
      (exit, fiber) => ZIO.succeed(Right((toFiber(fiber), toOutcome(exit))))
    )

  final override def both[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    fa.interruptible zipPar fb.interruptible

  final override def guarantee[A](fa: F[A], fin: F[Unit]): F[A] =
    fa.ensuring(fin.orDieWith(fiberFailure))

  final override def bracket[A, B](acquire: F[A])(use: A => F[B])(release: A => F[Unit]): F[B] =
    acquire.bracket(release.andThen(_.orDieWith(fiberFailure)), use)

  override val unique: F[Unique.Token] =
    ZIO.effectTotal(new Unique.Token)
}

private object ZioConcurrent {
  @inline def toOutcome[R, E, A](exit: Exit[E, A]): Outcome[ZIO[R, E, *], E, A] =
    exit match {
      case Exit.Success(value) =>
        Outcome.Succeeded[ZIO[R, E, *], E, A](ZIO.succeed(value))
      case Exit.Failure(cause) if cause.interrupted =>
        Outcome.Canceled[ZIO[R, E, *], E, A]()
      case Exit.Failure(cause) =>
        cause.failureOrCause match {
          case Left(error)  => Outcome.Errored[ZIO[R, E, *], E, A](error)
          case Right(cause) => Outcome.Succeeded[ZIO[R, E, *], E, A](ZIO.halt(cause))
        }
    }
}

private class ZioTemporal[R <: Clock, E] extends ZioConcurrent[R, E] with GenTemporal[ZIO[R, E, *], E] {

  final override def sleep(time: FiniteDuration): F[Unit] =
    clock.sleep(Duration.fromScala(time))

  final override val monotonic: F[FiniteDuration] =
    nanoTime.map(FiniteDuration(_, NANOSECONDS))

  final override val realTime: F[FiniteDuration] =
    currentTime(MILLISECONDS).map(FiniteDuration(_, MILLISECONDS))
}

private class ZioAsync[R <: Clock with Blocking] extends ZioTemporal[R, Throwable] with Async[RIO[R, *]] {

  final override def evalOn[A](fa: F[A], ec: ExecutionContext): F[A] =
    fa.on(ec)

  final override val executionContext: F[ExecutionContext] =
    ZIO.executor.map(_.asEC)

  final override val unique: F[Unique.Token] =
    ZIO.effectTotal(new Unique.Token)

  final override def cont[K, Q](body: Cont[F, K, Q]): F[Q] =
    Async.defaultCont(body)(this)

  final override def suspend[A](hint: Sync.Type)(thunk: => A): F[A] =
    hint match {
      case Sync.Type.Delay                                           => ZIO.effect(thunk)
      case Sync.Type.Blocking                                        => effectBlocking(thunk)
      case Sync.Type.InterruptibleOnce | Sync.Type.InterruptibleMany => effectBlockingInterrupt(thunk)
    }

  final override def delay[A](thunk: => A): F[A] =
    ZIO.effect(thunk)

  final override def defer[A](thunk: => F[A]): F[A] =
    ZIO.effectSuspend(thunk)

  final override def blocking[A](thunk: => A): F[A] =
    effectBlocking(thunk)

  final override def interruptible[A](many: Boolean)(thunk: => A): F[A] =
    effectBlockingInterrupt(thunk)

  final override def async[A](k: (Either[Throwable, A] => Unit) => F[Option[F[Unit]]]): F[A] =
    Promise.make[Nothing, Unit].flatMap { promise =>
      ZIO.effectAsyncM { register =>
        k(either => register(promise.await *> ZIO.fromEither(either))) *> promise.succeed(())
      }
    }

  final override def async_[A](k: (Either[Throwable, A] => Unit) => Unit): F[A] =
    ZIO.effectAsync(register => k(register.compose(fromEither)))

  final override def fromFuture[A](fut: F[Future[A]]): F[A] =
    fut.flatMap(f => ZIO.fromFuture(_ => f))

  final override def never[A]: F[A] =
    ZIO.never
}

private class ZioRuntimeTemporal[E](implicit runtime: Runtime[Clock])
    extends ZioConcurrent[Any, E]
    with GenTemporal[IO[E, *], E] {

  private[this] val underlying: GenTemporal[ZIO[Clock, E, *], E] = new ZioTemporal[Clock, E]
  private[this] val clock: Clock = runtime.environment

  final override def sleep(time: FiniteDuration): F[Unit] =
    underlying.sleep(time).provide(clock)

  final override val monotonic: F[FiniteDuration] =
    underlying.monotonic.provide(clock)

  final override val realTime: F[FiniteDuration] =
    underlying.realTime.provide(clock)
}

private class ZioRuntimeAsync(implicit runtime: Runtime[Clock with Blocking])
    extends ZioRuntimeTemporal[Throwable]
    with Async[Task] {

  private[this] val underlying: Async[RIO[Clock with Blocking, *]] = new ZioAsync[Clock with Blocking]
  private[this] val environment: Clock with Blocking = runtime.environment

  final override def evalOn[A](fa: F[A], ec: ExecutionContext): F[A] =
    underlying.evalOn(fa, ec).provide(environment)

  final override val executionContext: F[ExecutionContext] =
    underlying.executionContext.provide(environment)

  final override val unique: F[Unique.Token] =
    underlying.unique.provide(environment)

  final override def cont[K, Q](body: Cont[F, K, Q]): F[Q] =
    Async.defaultCont(body)(this)

  final override def suspend[A](hint: Sync.Type)(thunk: => A): F[A] =
    underlying.suspend(hint)(thunk).provide(environment)

  final override def delay[A](thunk: => A): F[A] =
    underlying.delay(thunk).provide(environment)

  final override def defer[A](thunk: => F[A]): F[A] =
    underlying.defer(thunk).provide(environment)

  final override def blocking[A](thunk: => A): F[A] =
    underlying.blocking(thunk).provide(environment)

  final override def interruptible[A](many: Boolean)(thunk: => A): F[A] =
    underlying.interruptible(many)(thunk).provide(environment)

  final override def async[A](k: (Either[Throwable, A] => Unit) => F[Option[F[Unit]]]): F[A] =
    underlying.async(k).provide(environment)

  final override def async_[A](k: (Either[Throwable, A] => Unit) => Unit): F[A] =
    underlying.async_(k).provide(environment)

  final override def fromFuture[A](fut: F[Future[A]]): F[A] =
    underlying.fromFuture(fut).provide(environment)

  final override def never[A]: F[A] =
    ZIO.never
}
