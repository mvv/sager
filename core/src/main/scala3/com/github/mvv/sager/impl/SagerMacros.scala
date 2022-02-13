package com.github.mvv.sager.impl

import com.github.mvv.sager.{Field, Record}
import com.github.mvv.typine.!:=
import com.github.mvv.typine.impl.TypineMacros
import izumi.reflect.macrortti.LightTypeTagRef.AbstractReference
import izumi.reflect.dottyreflection.TypeInspections

import scala.annotation.tailrec
import scala.quoted.*

object SagerMacros:
  private def unsafeNotFound(
      using qctx: Quotes)(labelRepr: qctx.reflect.TypeRepr, recordRepr: qctx.reflect.TypeRepr): qctx.reflect.Term = {
    import qctx.reflect.*
    Ref(Symbol.requiredMethod("com.github.mvv.sager.Record.NotFound.unsafeMake"))
      .appliedToTypes(List(labelRepr, recordRepr))
  }

  private def makeNotFound(using qctx: Quotes)(labelRepr: qctx.reflect.TypeRepr,
                                               record1Repr: qctx.reflect.TypeRepr,
                                               record2Repr: qctx.reflect.TypeRepr)(
      absent1Term: qctx.reflect.Term,
      absent2Term: qctx.reflect.Term): qctx.reflect.Term = {
    import qctx.reflect.*
    Ref(Symbol.requiredMethod("com.github.mvv.sager.Record.NotFound.make"))
      .appliedToTypes(List(labelRepr, record1Repr, record2Repr))
      .appliedToArgs(List(absent1Term, absent2Term))
  }

  private def summonAbsent(
      using qctx: Quotes)(labelRepr: qctx.reflect.TypeRepr, recordRepr: qctx.reflect.TypeRepr): qctx.reflect.Term = {
    import qctx.reflect.*
    Ref(Symbol.requiredMethod("scala.Predef.summon"))
      .appliedToType(TypeRepr.of[Record.Absent].appliedTo(List(labelRepr, recordRepr)))
  }

  private def unsafeFound(using qctx: Quotes)(labelRepr: qctx.reflect.TypeRepr,
                                              lowerRepr: qctx.reflect.TypeRepr,
                                              upperRepr: qctx.reflect.TypeRepr,
                                              valueRepr: qctx.reflect.TypeRepr,
                                              restRepr: qctx.reflect.TypeRepr,
                                              recordRepr: qctx.reflect.TypeRepr): qctx.reflect.Term = {
    import qctx.reflect.*
    Ref(Symbol.requiredMethod("com.github.mvv.sager.Record.Found.unsafeMake"))
      .appliedToTypes(List(labelRepr, lowerRepr, upperRepr, valueRepr, restRepr, recordRepr))
  }

  private def makeFound(using qctx: Quotes)(labelRepr: qctx.reflect.TypeRepr,
                                            lowerRepr: qctx.reflect.TypeRepr,
                                            upperRepr: qctx.reflect.TypeRepr,
                                            foundRecordRepr: qctx.reflect.TypeRepr,
                                            absentRecordRepr: qctx.reflect.TypeRepr)(
      foundTerm: qctx.reflect.Term,
      absentTerm: qctx.reflect.Term): qctx.reflect.Term = {
    import qctx.reflect.*
    Ref(Symbol.requiredMethod("com.github.mvv.sager.Record.Found.make"))
      .appliedToTypes(List(labelRepr, lowerRepr, upperRepr, foundRecordRepr, absentRecordRepr))
      .appliedToArgs(List(foundTerm, absentTerm))
  }

  def deriveFound[L: Type, VL: Type, VU: Type, R <: Record: Type](
      using qctx: Quotes): Expr[Record.Found[L, VL, VU, R]] =
    import qctx.reflect.*
    val labelRepr = TypeRepr.of[L]
    val recordRepr = TypeRepr.of[R]
    val (fields, restRepr) = deconstructRecordType(recordRepr)
    val (sameReprs, diffRepr, unknownRepr) =
      fields.foldLeft((Option.empty[(TypeRepr, TypeRepr)], Option.empty[TypeRepr], restRepr)) {
        case ((sameReprs, diffRepr, unknownRepr), (fieldLabelRepr, fieldValueRepr)) =>
          val fieldRepr = TypeRepr.of[Field].appliedTo(List(fieldLabelRepr, fieldValueRepr))
          Implicits.search(TypeRepr.of[=:=].appliedTo(List(labelRepr, fieldLabelRepr))) match
            case _: ImplicitSearchSuccess =>
              (Some(sameReprs.fold((fieldRepr, fieldValueRepr))((sameRepr, valueRepr) =>
                 (AndType(sameRepr, fieldRepr), AndType(valueRepr, fieldValueRepr)))),
               diffRepr,
               unknownRepr)
            case _: ImplicitSearchFailure =>
              Implicits.search(TypeRepr.of[!:=].appliedTo(List(labelRepr, fieldLabelRepr))) match
                case _: ImplicitSearchSuccess =>
                  (sameReprs, Some(diffRepr.fold(fieldRepr)(AndType(_, fieldRepr))), unknownRepr)
                case _: ImplicitSearchFailure =>
                  (sameReprs, diffRepr, Some(unknownRepr.fold(fieldRepr)(AndType(_, fieldRepr))))
      }
    val absentInDiff = diffRepr.map(diffRepr => (diffRepr, unsafeNotFound(labelRepr, diffRepr)))
    sameReprs match
      case Some((sameRepr, valueRepr)) =>
        val foundInSame = unsafeFound(labelRepr, valueRepr, valueRepr, valueRepr, TypeRepr.of[Record], sameRepr)
        val (knownRepr, foundInKnown) = absentInDiff.fold((sameRepr, foundInSame)) { (diffRepr, absentInDiff) =>
          (AndType(sameRepr, diffRepr),
           makeFound(labelRepr, valueRepr, valueRepr, sameRepr, diffRepr)(foundInSame, absentInDiff))
        }
        unknownRepr match
          case Some(unknownRepr) =>
            val absentInUnknown = summonAbsent(labelRepr, unknownRepr)
            makeFound(labelRepr, valueRepr, valueRepr, knownRepr, unknownRepr)(foundInKnown, absentInUnknown)
              .asExprOf[Record.Found[L, VL, VU, R]]
          case None =>
            foundInKnown.asExprOf[Record.Found[L, VL, VU, R]]
      case None =>
        (absentInDiff, unknownRepr) match
          case (Some((diffRepr, absentInDiff)), Some(unknownRepr)) =>
            Implicits.search(TypeRepr.of[Record.FoundSome].appliedTo(List(labelRepr, unknownRepr))) match
              case success: ImplicitSearchSuccess =>
                val (lowerRepr, upperRepr) = success.tree.tpe.baseType(TypeRepr.of[Record.Found].typeSymbol) match
                  case AppliedType(conRepr, List(_, lowerRepr, upperRepr, _)) =>
                    (lowerRepr, upperRepr)
                makeFound(labelRepr, lowerRepr, upperRepr, unknownRepr, diffRepr)(success.tree, absentInDiff)
                  .asExprOf[Record.Found[L, VL, VU, R]]
              case failure: ImplicitSearchFailure =>
                report.throwError(
                  s"while looking for field ${Type.show[L]} in record ${Type.show[R]}: ${failure.explanation}")
          case _ =>
            report.throwError(s"could not prove that record ${Type.show[R]} has field ${Type.show[L]}")

  def deriveNotFound[L: Type, R <: Record: Type](using qctx: Quotes): Expr[Record.NotFound[L, R]] =
    import qctx.reflect.*
    val labelRepr = TypeRepr.of[L]
    val recordRepr = TypeRepr.of[R]
    val (fields, restRepr) = deconstructRecordType(recordRepr)
    val (diffRepr, unknownRepr) = fields.foldLeft((Option.empty[TypeRepr], restRepr)) {
      case ((diffRepr, unknownRepr), (fieldLabelRepr, fieldValueRepr)) =>
        val fieldRepr = TypeRepr.of[Field].appliedTo(List(fieldLabelRepr, fieldValueRepr))
        Implicits.search(TypeRepr.of[!:=].appliedTo(List(labelRepr, fieldLabelRepr))) match
          case _: ImplicitSearchSuccess =>
            (Some(diffRepr.fold(fieldRepr)(AndType(_, fieldRepr))), unknownRepr)
          case _: ImplicitSearchFailure =>
            (diffRepr, Some(unknownRepr.fold(fieldRepr)(AndType(_, fieldRepr))))
    }
    diffRepr match
      case Some(diffRepr) =>
        val absentInDiff = unsafeNotFound(labelRepr, diffRepr)
        unknownRepr match
          case Some(unknownRepr) =>
            val absentInUnknown = summonAbsent(labelRepr, unknownRepr)
            makeNotFound(labelRepr, diffRepr, unknownRepr)(absentInDiff, absentInUnknown)
              .asExprOf[Record.NotFound[L, R]]
          case None =>
            absentInDiff.asExprOf[Record.NotFound[L, R]]
      case None =>
        report.throwError(s"could not prove that record ${Type.show[R]} does not have field ${Type.show[L]}")

  private def deconstructRecordType(using qctx: Quotes)(recordRepr: qctx.reflect.TypeRepr)
      : (Seq[(qctx.reflect.TypeRepr, qctx.reflect.TypeRepr)], Option[qctx.reflect.TypeRepr]) =
    import qctx.reflect.*
    def linearize(left: TypeRepr): LazyList[TypeRepr] =
      LazyList(left).flatMap {
        case AndType(firstRepr, secondRepr) =>
          linearize(firstRepr.dealias) ++ linearize(secondRepr.dealias)
        case repr =>
          LazyList(repr)
      }
    @tailrec
    def loop(fields: Seq[(TypeRepr, TypeRepr)],
             rest: Option[TypeRepr],
             left: Seq[TypeRepr]): (Seq[(TypeRepr, TypeRepr)], Option[TypeRepr]) =
      left.headOption match
        case Some(tpe @ AppliedType(conRepr, List(labelRepr, valueRepr))) if conRepr =:= TypeRepr.of[Field] =>
          loop((labelRepr, valueRepr) +: fields, rest, left.tail)
        case Some(tpe) =>
          loop(fields, Some(rest.fold(tpe)(AndType(_, tpe))), left.tail)
        case None =>
          val left1 = rest.fold(left)(_ +: left)
          (fields, left1.headOption.map(left1.tail.foldLeft(_)(AndType(_, _))))
    loop(Nil, None, linearize(recordRepr.dealias.simplified))
