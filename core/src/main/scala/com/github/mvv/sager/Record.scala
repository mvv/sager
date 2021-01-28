package com.github.mvv.sager

import com.github.mvv.sager.impl.{SagerBlackBoxMacros, SagerWhiteBoxMacros}
import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag

import scala.language.experimental.macros

sealed trait Record extends Serializable
sealed trait Field[L, +V] extends Record

object Field {
  implicit final class FieldSyntax[L, V](val underlying: Field[L, V]) extends AnyVal {
    def value(implicit tag: Tag[L]): V = underlying.get[L](tag, Record.Select.make[L, V, Field[L, V]])
    def map[V1](f: V => V1)(implicit tag: Tag[L]): Field[L, V1] = Field[L](f(value))
  }

  final class Single[L](val dummy: Unit) extends AnyVal {
    def apply[V](value: V)(implicit tag: Tag[L]): Field[L, V] = Record.empty.add[L](value)
  }

  def apply[L]: Single[L] = new Single[L](())
}

object Record {
  final private case class Impl(fields: Map[LightTypeTag, Any]) extends Field[Any, Any]

  sealed trait Select[L, R <: Record] {
    type Value
    implicit val valueWitness: R <:< Field[L, Value]
  }
  object Select {
    private val singleton: Select[Any, Field[Any, Any]] = new Select[Any, Field[Any, Any]] {
      override type Value = Any
      implicit override val valueWitness: Field[Any, Any] <:< Field[Any, Any] = implicitly
    }
    def make[L, V, R <: Record](implicit witness: R <:< Field[L, V]): Select[L, R] { type Value = V } =
      singleton.asInstanceOf[Select[L, R] { type Value = V }]
    implicit def select[L, R <: Record](implicit witness: R <:< Field[L, Any]): Select[L, R] =
      macro SagerWhiteBoxMacros.select[L, R]
  }

  sealed trait NotFound[L, R <: Record] {
    def covary[S >: R <: Record]: NotFound[L, S]
  }
  sealed trait NotFoundLow {
    implicit def notFound[L, R <: Record]: NotFound[L, R] = macro SagerBlackBoxMacros.notFound[L, R]
  }
  object NotFound extends NotFoundLow {
    private val singleton: NotFound[Any, Record] = new NotFound[Any, Record] {
      def covary[S >: Record <: Record] = this
    }
    def unsafeMake[L, R <: Record]: NotFound[L, R] =
      singleton.asInstanceOf[NotFound[L, R]]
    def make[L, R1 <: Record, R2 <: Record](
        implicit notFound1: NotFound[L, R1],
        notFound2: NotFound[L, R2]): NotFound[L, R1 with R2] =
      unsafeMake[L, R1 with R2]
    implicit def emptyNotFound[L]: NotFound[L, Record] =
      unsafeMake[L, Record]
  }

  sealed trait Found[L, R <: Record] extends Select[L, R] {
    type Rest >: R <: Record
    implicit val restContainsNot: NotFound[L, Rest]
    implicit def restContains[S >: R <: Record](implicit notFound: NotFound[L, S]): Rest <:< S
    implicit def restFind[L1](implicit find1: Find[L1, R], notFound: NotFound[L, Field[L1, Any]]): Find[L1, Rest]
    implicit def restFound[L1](
        implicit found1: Found[L1, R],
        notFound: NotFound[L, Field[L1, Any]]): Found[L1, Rest] { type Value = found1.Value }
    implicit val reconstructWitness: Field[L, Value] with Rest =:= R
    // For Scala 2.12, where =:= is NOT a subtype of <:<
    val reconstructRelaxedWitness: Field[L, Value] with Rest <:< R
  }
  object Found {
    implicit def found[L, R <: Record](implicit witness: R <:< Field[L, Any]): Found[L, R] =
      macro SagerWhiteBoxMacros.found[L, R]
  }

  sealed trait FoundValue[L, V, R <: Record] extends Found[L, R] {
    final type Value = V
    implicit def restFindSupValue[L1, V1](
        implicit find1: FindSupValue[L1, V1, R],
        notFound: NotFound[L, Field[L1, Any]]): FindSupValue[L1, V1, Rest]
    implicit final def restFoundValue[L1, V1](
        implicit found1: FoundValue[L1, V1, R],
        notFound: NotFound[L, Field[L1, Any]]): FoundValue[L1, V1, Rest] =
      FoundValue.fromFound(restFound[L1])
  }
  sealed trait FoundValueLow {
    implicit def foundValue[L, V, R <: Record](implicit witness: R <:< Field[L, V]): FoundValue[L, V, R] =
      macro SagerWhiteBoxMacros.found[L, R]
  }
  object FoundValue extends FoundValueLow {
    private val singleton: FoundValue[Any, Any, Field[Any, Any]] = new FoundValue[Any, Any, Field[Any, Any]] {
      override type Rest = Record
      override val reconstructWitness: Field[Any, Any] with Record =:= Field[Any, Any] = implicitly
      override val reconstructRelaxedWitness: Field[Any, Any] with Record <:< Field[Any, Any] = implicitly
      override val valueWitness: Field[Any, Any] <:< Field[Any, Any] = implicitly
      override val restContainsNot: NotFound[Any, Record] = implicitly
      override def restContains[S >: Field[Any, Any] <: Record](implicit notFound: NotFound[Any, S]): Record <:< S =
        implicitly[Any <:< Any].asInstanceOf[Record <:< S]
      override def restFind[L1](
          implicit find1: Find[L1, Field[Any, Any]],
          notFound: NotFound[Any, Field[L1, Any]]): Find[L1, Rest] =
        find1.asInstanceOf[Find[L1, Rest]]
      override def restFindSupValue[L1, V1](
          implicit find1: FindSupValue[L1, V1, Field[Any, Any]],
          notFound: NotFound[Any, Field[L1, Any]]): FindSupValue[L1, V1, Rest] =
        find1.asInstanceOf[FindSupValue[L1, V1, Rest]]
      override def restFound[L1](
          implicit found1: Found[L1, Field[Any, Any]],
          notFound: NotFound[Any, Field[L1, Any]]): FoundValue[L1, found1.Value, Rest] =
        singleton.asInstanceOf[FoundValue[L1, found1.Value, Rest]]
    }
    def make[L, V, R <: Record, RS >: R <: Record](
        implicit constructWitness: Field[L, V] with RS =:= R,
        restContainsNot: NotFound[L, RS]): FoundValue[L, V, R] {
      type Rest = RS
    } =
      singleton.asInstanceOf[FoundValue[L, V, R] { type Rest = RS }]
    implicit def fromFound[L, V, R <: Record](implicit found1: Found[L, R]): FoundValue[L, found1.Value, R] {
      type Rest = found1.Rest
    } =
      found1.asInstanceOf[FoundValue[L, found1.Value, R] { type Rest = found1.Rest }]
  }

  type FoundSup[L, V, R <: Record] = FoundValue[L, _ >: V, R]
  type FoundSub[L, V, R <: Record] = FoundValue[L, _ <: V, R]

  sealed trait Find[L, R <: Record] { self =>
    type Rest >: R <: Record
    implicit def restContainsNot: NotFound[L, Rest]
    implicit def restContains[S >: R <: Record](implicit notFound: NotFound[L, S]): Rest <:< S
    implicit def restFind[L1](implicit find1: Find[L1, R], notFound: NotFound[L, Field[L1, Any]]): Find[L1, Rest]
    implicit def restFound[L1](
        implicit found1: Found[L1, R],
        notFound: NotFound[L, Field[L1, Any]]): Found[L1, Rest] { type Value = found1.Value }
  }
  sealed trait FindLow {
    implicit def notFoundCase[L, R <: Record](implicit notFound: NotFound[L, R]): Find[L, R] { type Rest = R } =
      FindSupValue.notFoundCase[L, R]
  }
  object Find extends FindLow {
    final case class NotFoundCase[L, R <: Record](notFound: NotFound[L, R]) extends FindSupValue[L, Any, R] {
      override type Rest = R
      override def restContainsNot: NotFound[L, Rest] = notFound
      override def restContains[S >: R <: Record](implicit notFound: NotFound[L, S]): Rest <:< S = implicitly
      override def reconstructWitness: Field[L, Any] with R <:< R = implicitly
      override def restFind[L1](implicit find1: Find[L1, R], notFound: NotFound[L, Field[L1, Any]]): Find[L1, Rest] =
        find1
      override def restFindSupValue[L1, V1](
          implicit find1: FindSupValue[L1, V1, R],
          notFound: NotFound[L, Field[L1, Any]]): FindSupValue[L1, V1, Rest] =
        find1
      override def restFound[L1](
          implicit found1: Found[L1, R],
          notFound: NotFound[L, Field[L1, Any]]): Found[L1, Rest] {
        type Value = found1.Value
      } = found1
    }
    final case class FoundCase[L, V, R <: Record](found: FoundValue[L, V, R]) extends FindSupValue[L, V, R] {
      override type Rest = found.Rest
      override def restContainsNot: NotFound[L, found.Rest] = found.restContainsNot
      override def restContains[S >: R <: Record](implicit notFound: NotFound[L, S]): found.Rest <:< S =
        found.restContains[S](notFound)
      override def reconstructWitness: Field[L, V] with found.Rest <:< R = found.reconstructRelaxedWitness
      override def restFind[L1](implicit find1: Find[L1, R], notFound: NotFound[L, Field[L1, Any]]): Find[L1, Rest] =
        found.restFind[L1]
      override def restFindSupValue[L1, V1](
          implicit find1: FindSupValue[L1, V1, R],
          notFound: NotFound[L, Field[L1, Any]]): FindSupValue[L1, V1, Rest] =
        found.restFindSupValue[L1, V1]
      override def restFound[L1](
          implicit found1: Found[L1, R],
          notFound: NotFound[L, Field[L1, Any]]): Found[L1, Rest] {
        type Value = found1.Value
      } = found.restFound[L1]
    }
    implicit def foundCase[L, R <: Record](implicit found: Found[L, R]): Find[L, R] { type Rest = found.Rest } =
      FindSupValue.foundCase[L, R]
  }

  sealed trait FindSupValue[L, -V, R <: Record] extends Find[L, R] {
    implicit def reconstructWitness: Field[L, V] with Rest <:< R
    implicit def restFindSupValue[L1, V1](
        implicit find1: FindSupValue[L1, V1, R],
        notFound: NotFound[L, Field[L1, Any]]): FindSupValue[L1, V1, Rest]
    implicit final def restFoundValue[L1, V1](
        implicit found1: FoundValue[L1, V1, R],
        notFound: NotFound[L, Field[L1, Any]]): FoundValue[L1, V1, Rest] =
      FoundValue.fromFound(restFound[L1])
  }
  sealed trait FindSupValueLow {
    private val notFoundCaseSingleton: Find.NotFoundCase[Any, Record] =
      Find.NotFoundCase(NotFound.unsafeMake[Any, Record])
    implicit def notFoundCase[L, R <: Record](
        implicit notFound: NotFound[L, R]): FindSupValue[L, Any, R] { type Rest = R } =
      notFoundCaseSingleton.asInstanceOf[FindSupValue[L, Any, R] { type Rest = R }]
  }
  object FindSupValue extends FindSupValueLow {
    private val foundCaseSingleton: Find.FoundCase[Any, Any, Field[Any, Any]] =
      Find.FoundCase[Any, Any, Field[Any, Any]](FoundValue.make[Any, Any, Field[Any, Any], Record])
    implicit def foundCase[L, R <: Record](
        implicit found: Found[L, R]): FindSupValue[L, found.Value, R] { type Rest = found.Rest } =
      foundCaseSingleton.asInstanceOf[FindSupValue[L, found.Value, R] { type Rest = found.Rest }]
  }

  final class AddSyntax[L, R <: Record](val record: R) extends AnyVal {
    def apply[V](value: V)(implicit tag: Tag[L], find: Find[L, R]): find.Rest with Field[L, V] =
      record match {
        case Impl(fields) => Impl(fields + (tag.tag -> value)).asInstanceOf[find.Rest with Field[L, V]]
      }
  }

  final class UpdateSyntax[L, R <: Record](val record: R) extends AnyVal {
    def apply[V1, V2](f: V1 => V2)(implicit tag: Tag[L], found: FoundSub[L, V1, R]): found.Rest with Field[L, V2] = {
      import found.valueWitness
      record.add[L](f(record.get[L]))
    }
  }

  final class UpdateMonoSyntax[L, R <: Record](val record: R) extends AnyVal {
    def apply[V](f: V => V)(implicit tag: Tag[L], found: FoundValue[L, V, R]): R = {
      import found.reconstructWitness
      record.update[L](f)
    }
  }

  final class FieldSyntax[L, V, R <: Record](val record: R) extends AnyVal {
    def value(implicit tag: Tag[L]): V =
      record match {
        case Impl(fields) => fields(tag.tag).asInstanceOf[V]
      }
    def map[V1](f: V => V1)(implicit tag: Tag[L], found: FoundSub[L, V, R]): found.Rest with Field[L, V1] =
      record.update[L](f)
    def mapMono(f: V => V)(implicit tag: Tag[L], found: FoundValue[L, V, R]): R =
      record.updateMono[L](f)
    def remove(implicit tag: Tag[L], find: Find[L, R]): find.Rest =
      record.remove[L]
  }

  implicit final class Syntax[R <: Record](val record: R) extends AnyVal {
    def get[L](implicit tag: Tag[L], select: Select[L, R]): select.Value =
      record match {
        case Impl(fields) => fields(tag.tag).asInstanceOf[select.Value]
      }
    def add[L]: AddSyntax[L, R] = new AddSyntax[L, R](record)
    def remove[L](implicit tag: Tag[L], find: Find[L, R]): find.Rest =
      record match {
        case Impl(fields) => Impl(fields - tag.tag).asInstanceOf[find.Rest]
      }
    def update[L]: UpdateSyntax[L, R] = new UpdateSyntax[L, R](record)
    def updateMono[L]: UpdateMonoSyntax[L, R] = new UpdateMonoSyntax[L, R](record)
    def field[L](implicit tag: Tag[L], select: Select[L, R]): FieldSyntax[L, select.Value, R] =
      new FieldSyntax[L, select.Value, R](record)
  }

  val empty: Record = Impl(Map.empty)
}
