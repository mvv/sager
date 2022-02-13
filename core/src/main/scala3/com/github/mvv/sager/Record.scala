package com.github.mvv.sager

import com.github.mvv.sager.impl.SagerMacros
import com.github.mvv.typine.!:=
import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag

import scala.annotation.implicitNotFound

sealed trait Record extends Serializable
sealed trait Field[L, +V] extends Record

object Field:
  extension [L, V](field: Field[L, V])
    def value(using tag: Tag[L]): V = field.get[L]
    def map[V1](f: V => V1)(using tag: Tag[L]): Field[L, V1] = Field[L](f(field.value))

  opaque type Make[L] = Unit
  extension [L, V](make: Make[L]) def apply(value: V)(using tag: Tag[L]): Field[L, V] = Record.empty.add[L](value)

  def apply[L]: Make[L] = ()

object Record:
  final private case class Impl(fields: Map[LightTypeTag, Any]) extends Field[Any, Any]

  @implicitNotFound("could not prove that record ${R} has field ${L}")
  sealed trait Present[L, +V, -R <: Record]:
    type Value <: V
    given valueWitness: (R <:< Field[L, Value])
    def exact: Present[L, Value, R]
  object Present:
    private val singleton = new Present[Any, Any, Field[Any, Any]]:
      override type Value = Any
      override given valueWitness: (Field[Any, Any] <:< Field[Any, Any]) = summon
      override def exact: Present[Any, Any, Field[Any, Any]] = this
      override def toString: String = "Record.Present"
    inline given make[L, V, R <: Record](using witness: R <:< Field[L, V]): (Present[L, V, R] { type Value = V }) =
      singleton.asInstanceOf[Present[L, V, R] { type Value = V }]

  type Select[L, -R <: Record] = Present[L, Any, R]

  @implicitNotFound("could not prove that record ${R} does not have field ${L}")
  sealed trait Absent[L, +R <: Record]:
    given distinct[L1](using Select[L1, R]): (L !:= L1)
  sealed trait AbsentLowest:
    inline given derive[L, R <: Record]: Absent[L, R] = ${ SagerMacros.deriveNotFound[L, R] }
  sealed trait AbsentLow extends AbsentLowest:
    inline given distinct[L1, L2, V2](using L1 !:= L2): Absent[L1, Field[L2, V2]] = NotFound.distinct[L1, L2, V2]
  object Absent extends AbsentLow:
    inline given empty[L]: Absent[L, Record] = NotFound.empty[L]
    inline def make[L, R1 <: Record, R2 <: Record](absent1: Absent[L, R1],
                                                   absent2: Absent[L, R2]): Absent[L, R1 with R2] =
      NotFound.make[L, R1, R2](absent1, absent2)

  @implicitNotFound("could not extract part of ${R} that does not contain field ${L}")
  sealed trait Find[L, -V, R <: Record]:
    type Rest >: R <: Record
    given restContainsNot: NotFound[L, Rest]
    given restContains[S <: Record](using R <:< S, Absent[L, S]): (Rest <:< S)
    given restFind[L1, V1](using L1 !:= L, Find[L1, V1, R]): Find[L1, V1, Rest]
    given restNotFound[L1](using L1 !:= L, Absent[L1, R]): NotFound[L1, Rest]
    given restFound[L1, VL, VU](
        using distinct: L1 !:= L,
        found: Found[L1, VL, VU, R]): (Found[L1, VL, VU, Rest] { type Value = found.Value })
    given reconstructWitness: (Field[L, V] with Rest <:< R)
  sealed trait FindLow:
    inline given notFound[L, R <: Record](using witness: NotFound[L, R]): (Find[L, Any, R] { type Rest = R }) = witness
  object Find extends FindLow:
    inline given found[L, R <: Record](
        using witness: FoundSome[L, R]): (Find[L, witness.Value, R] { type Rest = witness.Rest }) = witness.exact

  @implicitNotFound("could not split ${R} into field ${L} with value >: ${VL} <: ${VU} and the rest")
  sealed trait Found[L, -VL, +VU, R <: Record] extends Find[L, VL, R] with Present[L, VU, R]:
    self =>
    override type Value >: VL <: VU
    override given reconstructWitness: (Field[L, Value] with Rest =:= R)
    override def exact: Found[L, Value, Value, R] { type Rest = self.Rest }
  sealed trait FoundLowest:
    transparent inline given derive[L, VL, VU, R <: Record]: Found[L, VL, VU, R] = ${
      SagerMacros.deriveFound[L, VL, VU, R]
    }
  sealed trait FoundLower extends FoundLowest:
    transparent inline given deriveSub[L, V, R <: Record]: FoundSub[L, V, R] = ${
      SagerMacros.deriveFound[L, Nothing, V, R]
    }
  sealed trait FoundLow extends FoundLower:
    transparent inline given deriveSome[L, R <: Record]: FoundSome[L, R] = ${
      SagerMacros.deriveFound[L, Nothing, Any, R]
    }
  object Found extends FoundLow:
    private val singleton = new Found[Any, Nothing, Any, Field[Any, Any]]:
      type Value = Any
      type Rest = Record
      override given valueWitness: (Field[Any, Any] <:< Field[Any, Any]) =
        reconstructWitness
      override given restContainsNot: NotFound[Any, Record] =
        summon[NotFound[Any, Record]]
      override given restContains[S <: Record](using sup: Field[Any, Any] <:< S, absent: Absent[Any, S]): (Rest <:< S) =
        sup.asInstanceOf[Rest <:< S]
      override given restFind[L1, V1](
          using distinct: L1 !:= Any,
          find: Find[L1, V1, Field[Any, Any]]): Find[L1, V1, Rest] =
        find.asInstanceOf[Find[L1, V1, Rest]]
      override given restNotFound[L1](
          using distinct: L1 !:= Any,
          absent: Absent[L1, Field[Any, Any]]): NotFound[L1, Rest] =
        absent.asInstanceOf[NotFound[L1, Rest]]
      override given restFound[L1, VL, VU](
          using distinct: L1 !:= Any,
          found: Found[L1, VL, VU, Field[Any, Any]]): (Found[L1, VL, VU, Rest] { type Value = found.Value }) =
        asInstanceOf[Found[L1, VL, VU, Rest] { type Value = found.Value }]
      override given reconstructWitness: (Field[Any, Value] with Rest =:= Field[Any, Any]) =
        summon[Field[Any, Any] with Record =:= Field[Any, Any]]
      override def exact: Found[Any, Any, Any, Field[Any, Any]] { type Rest = Record } =
        asInstanceOf[Found[Any, Any, Any, Field[Any, Any]] { type Rest = Record }]
      override def toString: String = "Record.Found"
    def unsafeMake[L, VL, VU, V >: VL <: VU, RT <: Record, R <: RT]
        : Found[L, VL, VU, R] { type Value = V; type Rest = RT } =
      singleton.asInstanceOf[Found[L, VL, VU, R] { type Value = V; type Rest = RT }]
    inline given field[L1, L2, V](
        using L1 =:= L2): (Found[L1, V, V, Field[L2, V]] { type Value = V; type Rest = Record }) =
      unsafeMake[L1, V, V, V, Record, Field[L2, V]]
    inline def make[L, VL, VU, R1 <: Record, R2 <: Record](
        found: Found[L, VL, VU, R1],
        absent: Absent[L, R2]): Found[L, VL, VU, R1 & R2] { type Value = found.Value; type Rest = found.Rest & R2 } =
      unsafeMake[L, VL, VU, found.Value, found.Rest & R2, R1 & R2]

  type FoundSome[L, R <: Record] = Found[L, Nothing, Any, R]
  type FoundSup[L, V, R <: Record] = Found[L, V, Any, R]
  type FoundSub[L, V, R <: Record] = Found[L, Nothing, V, R]

  @implicitNotFound("could not prove that record ${R} does not have field ${L}")
  sealed trait NotFound[L, R <: Record] extends Find[L, Any, R] with Absent[L, R]:
    final override type Rest = R
  object NotFound:
    private val singleton = new NotFound[Any, Record]:
      override given restContainsNot: NotFound[Any, Rest] = this
      override given restContains[S <: Record](using sup: Record <:< S, absent: Absent[Any, S]): (Record <:< S) = sup
      override given restFind[L1, V1](using distinct: L1 !:= Any, find: Find[L1, V1, Record]): Find[L1, V1, Rest] =
        find
      override given restNotFound[L1](using distinct: L1 !:= Any, absent: Absent[L1, Record]): NotFound[L1, Rest] =
        absent.asInstanceOf[NotFound[L1, Rest]]
      override given restFound[L1, VL, VU](
          using distinct: L1 !:= Any,
          found: Found[L1, VL, VU, Record]): (Found[L1, VL, VU, Rest] { type Value = found.Value }) =
        found
      override given reconstructWitness: (Field[Any, Any] with Record <:< Record) =
        summon[Field[Any, Any] with Record <:< Record]
      override given distinct[L1](using Select[L1, Record]): (Any !:= L1) = !:=.unsafeMake[Any, L1]
      override def toString: String = "Record.NotFound"
    def unsafeMake[L, R <: Record]: NotFound[L, R] = singleton.asInstanceOf[NotFound[L, R]]
    def empty[L]: NotFound[L, Record] = singleton.asInstanceOf[NotFound[L, Record]]
    def distinct[L1, L2, V2](using L1 !:= L2): NotFound[L1, Field[L2, V2]] =
      singleton.asInstanceOf[NotFound[L1, Field[L2, V2]]]
    inline given absent[L, R <: Record](using absent: Absent[L, R]): NotFound[L, R] =
      absent.asInstanceOf[NotFound[L, R]]
    def make[L, R1 <: Record, R2 <: Record](absent1: Absent[L, R1], absent2: Absent[L, R2]): NotFound[L, R1 with R2] =
      unsafeMake[L, R1 with R2]

  extension [R <: Record](record: R)
    def get[L](using tag: Tag[L], present: Select[L, R]): present.Value =
      record match
        case Impl(fields) => fields(tag.tag).asInstanceOf[present.Value]
    def add[L]: AddSyntax[L, R] = new AddSyntax[L, R](record)
    def remove[L](using tag: Tag[L], find: Find[L, Nothing, R]): find.Rest =
      record match
        case Impl(fields) => Impl(fields - tag.tag).asInstanceOf[find.Rest]
    def update[L](using found: FoundSome[L, R]): UpdateSyntax[L, found.Value, found.Rest, R] =
      new UpdateSyntax[L, found.Value, found.Rest, R](record)
    def updateMono[L](using found: FoundSome[L, R]): UpdateMonoSyntax[L, found.Value, R] =
      new UpdateMonoSyntax[L, found.Value, R](record)

  final class AddSyntax[L, R <: Record](val record: R) extends AnyVal:
    def apply[V](value: V)(using tag: Tag[L], find: Find[L, Nothing, R]): Field[L, V] with find.Rest =
      record match
        case Impl(fields) => Impl(fields + (tag.tag -> value)).asInstanceOf[Field[L, V] with find.Rest]

  final class UpdateSyntax[L, V, Rest <: Record, R <: Record](val record: R) extends AnyVal:
    def apply[V1](f: V => V1)(using tag: Tag[L]): Field[L, V1] with Rest =
      record match
        case Impl(fields) =>
          Impl(fields.updated(tag.tag, (v: Any) => f(v.asInstanceOf[V]))).asInstanceOf[Field[L, V1] with Rest]

  final class UpdateMonoSyntax[L, V, R <: Record](val record: R) extends AnyVal:
    def apply(f: V => V)(using tag: Tag[L]): R =
      record match
        case Impl(fields) => Impl(fields.updated(tag.tag, (v: Any) => f(v.asInstanceOf[V]))).asInstanceOf[R]

  val empty: Record = Impl(Map.empty)
