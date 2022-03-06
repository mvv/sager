package com.github.mvv.sager.impl

import com.github.mvv.sager.Record
import com.github.mvv.typine.impl.TypineMacros

import scala.reflect.macros.whitebox

class SagerWhiteBoxMacros(val c: whitebox.Context) extends SagerMacroUtils {
  import c.universe._

  protected val baseEqType = c.typeOf[=:=[Any, Any]]
  protected val baseFoundType = c.typeOf[Record.Found[Any, Any, Any, Record]]
  protected val anyType = c.typeOf[Any]
  protected val nothingType = c.typeOf[Nothing]

  private def createEqType(tpe1: c.Type, tpe2: c.Type): Type = {
    val TypeRef(eqPre, eqSym, _) = baseEqType
    c.internal.typeRef(eqPre, eqSym, List(tpe1, tpe2))
  }

  private def createFoundType(labelType: c.Type, recordType: c.Type): Type = {
    val TypeRef(foundPre, foundSym, _) = baseFoundType
    c.internal.typeRef(foundPre, foundSym, List(labelType, nothingType, anyType, recordType))
  }

  def select[L: WeakTypeTag, R <: Record: WeakTypeTag](witness: Tree): Tree = {
    val labelType = weakTypeTag[L].tpe
    val recordType = weakTypeTag[R].tpe
    val refineTree =
      q"""
         {
           def refine[V](x: _root_.com.github.mvv.sager.Field[$labelType, V]): V = ???
           refine((???): $recordType)
         }
       """
    val valueType = c.typecheck(refineTree).tpe
    q"_root_.com.github.mvv.sager.Record.Present.make[$labelType, $valueType, $recordType]"
  }

  def found[L: WeakTypeTag, VL, VU, R <: Record: WeakTypeTag]: Tree = {
    val labelType = weakTypeTag[L].tpe
    val recordType = weakTypeTag[R].tpe

    val (fields, rest) = deconstructRecordType(recordType)
    val (sameType, diffType, unknownType) =
      fields.foldLeft(
        (Option.empty[(Either[Tree, Type], Either[Tree, Type])],
         Option.empty[Either[Tree, Type]],
         rest.map(Right(_): Either[Tree, Type]))) {
        case ((sameType, diffType, unknownType), (fieldLabelType, fieldValueType)) =>
          val fieldType = createFieldType(fieldLabelType, fieldValueType)
          val eqType = createEqType(labelType, fieldLabelType)
          if (c.inferImplicitValue(eqType) != EmptyTree) {
            (withType(sameType.map(_._1), fieldType).flatMap(tpe =>
               withType(sameType.map(_._2), fieldValueType).map((tpe, _))),
             diffType,
             unknownType)
          } else if (TypineMacros.searchUnequal(c)(labelType, fieldLabelType) != EmptyTree) {
            (sameType, withType(diffType, fieldType), unknownType)
          } else {
            (sameType, diffType, withType(unknownType, fieldType))
          }
      }
    val absentInDiff = diffType
      .map(toType)
      .map(diffType => (diffType, q"_root_.com.github.mvv.sager.Record.NotFound.unsafeMake[$labelType, $diffType]"))
    sameType.map { case (sameType, valueType) => (toType(sameType), toType(valueType)) } match {
      case Some((sameType, valueType)) =>
        val foundInSame =
          q"_root_.com.github.mvv.sager.Record.Found.unsafeMake[$labelType, $valueType, $valueType, $valueType, $baseRecordType, $sameType]"
        val (knownType, foundInKnown) = absentInDiff.fold((tq"$sameType", foundInSame)) {
          case (diffType, absentInDiff) =>
            (tq"$diffType with $sameType",
             q"_root_.com.github.mvv.sager.Record.Found.make[$labelType, $valueType, $valueType, $sameType, $diffType]($foundInSame, $absentInDiff)")
        }
        unknownType.map(toType) match {
          case Some(unknownType) =>
            q"""
               _root_.com.github.mvv.sager.Record.Found.make[$labelType, $valueType, $valueType, $knownType, $unknownType](
                 $foundInKnown,
                 implicitly[_root_.com.github.mvv.sager.Record.Absent[$labelType, $unknownType]]
               )
             """
          case None =>
            foundInKnown
        }
      case None =>
        (absentInDiff, unknownType.map(toType)) match {
          case (Some((diffType, absentInDiff)), Some(unknownType)) =>
            c.inferImplicitValue(createFoundType(labelType, unknownType)) match {
              case q"com.github.mvv.sager.Record.Found.found[$_, $_]" =>
                EmptyTree
              case tree if tree == EmptyTree =>
                EmptyTree
              case tree =>
                val (lowerType, upperType) = tree.tpe.baseType(baseFoundType.typeSymbol) match {
                  case AppliedTypeTree(_, List(_, lowerType, upperType, _)) =>
                    (lowerType, upperType)
                }
                q"""
                   _root_.com.github.mvv.sager.Record.Found.make[$labelType, $lowerType, $upperType, $unknownType, $diffType](
                     $tree, $absentInDiff)
                 """
            }
          case _ =>
            EmptyTree
        }
    }
  }
}
