package com.github.mvv.sager

import com.github.mvv.sager.impl.{SagerBlackBoxMacros, SagerWhiteBoxMacros}
import com.github.mvv.typine.!:=
import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag

import scala.annotation.implicitNotFound
import scala.language.experimental.macros

sealed trait Record extends Serializable
sealed trait Field[L, +V] extends Record

object Field {
  implicit final class FieldSyntax[L, V](val underlying: Field[L, V]) extends AnyVal {
    def value(implicit tag: Tag[L]): V = underlying.get[L](tag, Record.Present.make[L, V, Field[L, V]])
    def map[V1](f: V => V1)(implicit tag: Tag[L]): Field[L, V1] = Field[L](f(value))
  }

  final class Single[L](val dummy: Unit) extends AnyVal {
    def apply[V](value: V)(implicit tag: Tag[L]): Field[L, V] = Record.empty.add[L](value)
  }

  def apply[L]: Single[L] = new Single[L](())
}

object Record {
  final private case class Impl(fields: Map[LightTypeTag, Any]) extends Field[Any, Any] {
    override def toString: String =
      fields.iterator.map { case (key, value) => s"$key -> $value" }.mkString("Record(", ", ", ")")
  }

  @implicitNotFound("could not prove that record ${R} has field ${L}")
  sealed trait Present[L, +V, -R <: Record] {
    type Value <: V
    implicit def valueWitness: R <:< Field[L, Value]
    def exact: Present[L, Value, R]
  }
  object Present {
    private val singleton: Present[Any, Any, Field[Any, Any]] = new Present[Any, Any, Field[Any, Any]] {
      override type Value = Any
      implicit override val valueWitness: Field[Any, Any] <:< Field[Any, Any] = implicitly
      override def exact: Present[Any, Any, Field[Any, Any]] = this
      override def toString: String = "Record.Present"
    }
    def make[L, V, R <: Record](implicit witness: R <:< Field[L, V]): Present[L, V, R] { type Value = V } =
      singleton.asInstanceOf[Present[L, V, R] { type Value = V }]
    implicit def select[L, R <: Record](implicit witness: R <:< Field[L, Any]): Select[L, R] =
      macro SagerWhiteBoxMacros.select[L, R]
  }

  type Select[L, -R <: Record] = Present[L, Any, R]

  @implicitNotFound("could not prove that record ${R} does not have field ${L}")
  sealed trait Absent[L, +R <: Record] {
    implicit def distinct[L1](implicit present: Select[L1, R]): L !:= L1
  }
  sealed trait AbsentLowest {
    implicit def absent[L, R <: Record]: Absent[L, R] = macro SagerBlackBoxMacros.notFound[L, R]
  }
  sealed trait AbsentLow extends AbsentLowest {
    implicit def distinct[L1, L2, V2](implicit witness: L1 !:= L2): Absent[L1, Field[L2, V2]] =
      NotFound.distinct[L1, L2, V2]
  }
  object Absent extends AbsentLow {
    implicit def empty[L]: Absent[L, Record] = NotFound.empty[L]
    def make[L, R1 <: Record, R2 <: Record](absent1: Absent[L, R1], absent2: Absent[L, R2]): Absent[L, R1 with R2] =
      NotFound.make[L, R1, R2](absent1, absent2)
  }

  @implicitNotFound("could not extract part of ${R} that does not contain field ${L}")
  sealed trait Find[L, -V, R <: Record] {
    type Rest >: R <: Record
    implicit def restContainsNot: NotFound[L, Rest]
    implicit def restContains[S <: Record](implicit sup: R <:< S, absent: Absent[L, S]): Rest <:< S
    implicit def restFind[L1, V1](implicit distinct: L1 !:= L, find: Find[L1, V1, R]): Find[L1, V1, Rest]
    implicit def restNotFound[L1](implicit distinct: L1 !:= L, absent: Absent[L1, R]): NotFound[L1, Rest]
    implicit def restFound[L1, VL, VU >: VL](
        implicit distinct: L1 !:= L,
        found: Found[L1, VL, VU, R]): Found[L1, VL, VU, Rest] { type Value = found.Value }
    implicit def reconstructWitness: Field[L, V] with Rest <:< R
  }
  sealed trait FindLow {
    implicit def notFound[L, R <: Record](implicit witness: NotFound[L, R]): Find[L, Any, R] { type Rest = R } = witness
  }
  object Find extends FindLow {
    implicit def found[L, R <: Record](
        implicit witness: FoundSome[L, R]): Find[L, witness.Value, R] { type Rest = witness.Rest } = witness.exact
  }

  type FindAny[L, R <: Record] = Find[L, Nothing, R]

  @implicitNotFound("could not split ${R} into field ${L} with value >: ${VL} <: ${VU} and the rest")
  sealed trait Found[L, -VL, +VU >: VL, R <: Record] extends Find[L, VL, R] with Present[L, VU, R] { self =>
    override type Value >: VL <: VU
    implicit override def reconstructWitness: Field[L, Value] with Rest =:= R
    override def exact: Found[L, Value, Value, R] { type Rest = self.Rest }
  }
  sealed trait FoundLow {
    implicit def found[L, VL, VU >: VL, R <: Record]: Found[L, VL, VU, R] =
      macro SagerWhiteBoxMacros.found[L, VL, VU, R]
  }
  object Found extends FoundLow {
    private val singleton = new Found[Any, Nothing, Any, Field[Any, Any]] {
      type Value = Any
      type Rest = Record
      implicit override def valueWitness: (Field[Any, Any] <:< Field[Any, Any]) =
        reconstructWitness
      implicit override def restContainsNot: NotFound[Any, Record] =
        implicitly[NotFound[Any, Record]]
      implicit override def restContains[S <: Record](
          implicit sup: Field[Any, Any] <:< S,
          absent: Absent[Any, S]): (Rest <:< S) =
        sup.asInstanceOf[Rest <:< S]
      implicit override def restFind[L1, V1](
          implicit distinct: L1 !:= Any,
          find: Find[L1, V1, Field[Any, Any]]): Find[L1, V1, Rest] =
        find.asInstanceOf[Find[L1, V1, Rest]]
      implicit override def restNotFound[L1](
          implicit distinct: L1 !:= Any,
          absent: Absent[L1, Field[Any, Any]]): NotFound[L1, Rest] =
        absent.asInstanceOf[NotFound[L1, Rest]]
      implicit override def restFound[L1, VL, VU >: VL](
          implicit distinct: L1 !:= Any,
          found: Found[L1, VL, VU, Field[Any, Any]]): Found[L1, VL, VU, Rest] { type Value = found.Value } =
        asInstanceOf[Found[L1, VL, VU, Rest] { type Value = found.Value }]
      implicit override def reconstructWitness: Field[Any, Value] with Rest =:= Field[Any, Any] =
        implicitly[Field[Any, Any] with Record =:= Field[Any, Any]]
      override def exact: Found[Any, Any, Any, Field[Any, Any]] { type Rest = Record } =
        asInstanceOf[Found[Any, Any, Any, Field[Any, Any]] { type Rest = Record }]
      override def toString: String = "Record.Found"
    }
    def unsafeMake[L, VL, VU >: VL, V >: VL <: VU, RT <: Record, R <: RT]
        : Found[L, VL, VU, R] { type Value = V; type Rest = RT } =
      singleton.asInstanceOf[Found[L, VL, VU, R] { type Value = V; type Rest = RT }]
    implicit def field[L1, L2, V](
        implicit distinct: L1 =:= L2): Found[L1, V, V, Field[L2, V]] { type Value = V; type Rest = Record } =
      unsafeMake[L1, V, V, V, Record, Field[L2, V]]
    def make[L, VL, VU >: VL, R1 <: Record, R2 <: Record](found: Found[L, VL, VU, R1], absent: Absent[L, R2])
        : Found[L, VL, VU, R1 with R2] { type Value = found.Value; type Rest = found.Rest with R2 } =
      unsafeMake[L, VL, VU, found.Value, found.Rest with R2, R1 with R2]
  }

  type FoundSome[L, R <: Record] = Found[L, Nothing, Any, R]
  type FoundSup[L, V, R <: Record] = Found[L, V, Any, R]
  type FoundSub[L, V, R <: Record] = Found[L, Nothing, V, R]

  @implicitNotFound("could not prove that record ${R} does not have field ${L}")
  sealed trait NotFound[L, R <: Record] extends Find[L, Any, R] with Absent[L, R] {
    final override type Rest = R
  }
  object NotFound {
    private val singleton = new NotFound[Any, Record] {
      implicit override def restContainsNot: NotFound[Any, Rest] = this
      implicit override def restContains[S <: Record](
          implicit sup: Record <:< S,
          absent: Absent[Any, S]): Record <:< S = sup
      implicit override def restFind[L1, V1](
          implicit distinct: L1 !:= Any,
          find: Find[L1, V1, Record]): Find[L1, V1, Rest] =
        find
      implicit override def restNotFound[L1](
          implicit distinct: L1 !:= Any,
          absent: Absent[L1, Record]): NotFound[L1, Rest] =
        absent.asInstanceOf[NotFound[L1, Rest]]
      implicit override def restFound[L1, VL, VU >: VL](
          implicit distinct: L1 !:= Any,
          found: Found[L1, VL, VU, Record]): Found[L1, VL, VU, Rest] { type Value = found.Value } =
        found
      implicit override def reconstructWitness: (Field[Any, Any] with Record <:< Record) =
        implicitly[Field[Any, Any] with Record <:< Record]
      implicit override def distinct[L1](implicit present: Select[L1, Record]): Any !:= L1 = !:=.unsafeMake[Any, L1]
      override def toString: String = "Record.NotFound"
    }
    def unsafeMake[L, R <: Record]: NotFound[L, R] = singleton.asInstanceOf[NotFound[L, R]]
    def empty[L]: NotFound[L, Record] = singleton.asInstanceOf[NotFound[L, Record]]
    def distinct[L1, L2, V2](implicit witness: L1 !:= L2): NotFound[L1, Field[L2, V2]] =
      singleton.asInstanceOf[NotFound[L1, Field[L2, V2]]]
    implicit def absent[L, R <: Record](implicit absent: Absent[L, R]): NotFound[L, R] =
      absent.asInstanceOf[NotFound[L, R]]
    def make[L, R1 <: Record, R2 <: Record](absent1: Absent[L, R1], absent2: Absent[L, R2]): NotFound[L, R1 with R2] =
      unsafeMake[L, R1 with R2]
  }

  final class AddSyntax[L, R <: Record](val record: R) extends AnyVal {
    def apply[V](value: V)(implicit tag: Tag[L], find: FindAny[L, R]): find.Rest with Field[L, V] =
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
    def apply[V](f: V => V)(implicit tag: Tag[L], found: Found[L, V, V, R]): R = {
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
    def mapMono(f: V => V)(implicit tag: Tag[L], found: Found[L, V, V, R]): R =
      record.updateMono[L](f)
    def remove(implicit tag: Tag[L], find: FindAny[L, R]): find.Rest =
      record.remove[L]
  }

  implicit final class Syntax[R <: Record](val record: R) extends AnyVal {
    def get[L](implicit tag: Tag[L], select: Select[L, R]): select.Value =
      record match {
        case Impl(fields) => fields(tag.tag).asInstanceOf[select.Value]
      }
    def add[L]: AddSyntax[L, R] = new AddSyntax[L, R](record)
    def remove[L](implicit tag: Tag[L], find: FindAny[L, R]): find.Rest =
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
