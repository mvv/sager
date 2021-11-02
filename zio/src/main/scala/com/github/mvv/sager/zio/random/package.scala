package com.github.mvv.sager.zio

import _root_.zio.{Chunk, ULayer, URIO, URLayer, ZLayer, random => zioRandom}

import java.util.UUID

package object random {
  type Random = Haz[zioRandom.Random.Service]
  type RandomEnv[A <: zioRandom.Random.Service] = Env[zioRandom.Random.Service, A]
  val Random: zioRandom.Random.type = zioRandom.Random

  object SagerRandom {
    val any: URLayer[Random, Random] = ZLayer.requires[Random]
    val live: ULayer[Random] = ZLayer.succeedHaz(Random.Service.live)
  }

  val nextBoolean: URIO[Random, Boolean] =
    URIO.accessM(_.value.nextBoolean)
  def nextBytes(length: => Int): URIO[Random, Chunk[Byte]] =
    URIO.accessM(_.value.nextBytes(length))
  val nextDouble: URIO[Random, Double] =
    URIO.accessM(_.value.nextDouble)
  def nextDoubleBetween(minInclusive: Double, maxExclusive: Double): URIO[Random, Double] =
    URIO.accessM(_.value.nextDoubleBetween(minInclusive, maxExclusive))
  val nextFloat: URIO[Random, Float] =
    URIO.accessM(_.value.nextFloat)
  def nextFloatBetween(minInclusive: Float, maxExclusive: Float): URIO[Random, Float] =
    URIO.accessM(_.value.nextFloatBetween(minInclusive, maxExclusive))
  val nextGaussian: URIO[Random, Double] =
    URIO.accessM(_.value.nextGaussian)
  val nextInt: URIO[Random, Int] =
    URIO.accessM(_.value.nextInt)
  def nextIntBetween(minInclusive: Int, maxExclusive: Int): URIO[Random, Int] =
    URIO.accessM(_.value.nextIntBetween(minInclusive, maxExclusive))
  def nextIntBounded(n: => Int): URIO[Random, Int] =
    URIO.accessM(_.value.nextIntBounded(n))
  val nextLong: URIO[Random, Long] =
    URIO.accessM(_.value.nextLong)
  def nextLongBetween(minInclusive: Long, maxExclusive: Long): URIO[Random, Long] =
    URIO.accessM(_.value.nextLongBetween(minInclusive, maxExclusive))
  def nextLongBounded(n: => Long): URIO[Random, Long] =
    URIO.accessM(_.value.nextLongBounded(n))
  val nextUUID: URIO[Random, UUID] =
    URIO.accessM(_.value.nextUUID)
  val nextPrintableChar: URIO[Random, Char] =
    URIO.accessM(_.value.nextPrintableChar)
  def nextString(length: => Int): URIO[Random, String] =
    URIO.accessM(_.value.nextString(length))
  def setSeed(seed: Long): URIO[Random, Unit] =
    URIO.accessM(_.value.setSeed(seed))
  def shuffle[A](list: => List[A]): URIO[Random, List[A]] =
    URIO.accessM(_.value.shuffle(list))
}
