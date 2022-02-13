package com.github.mvv.sager.impl

import com.github.mvv.sager.{Field, Record}

import scala.reflect.macros.blackbox

trait SagerMacroUtils {
  val c: blackbox.Context
  import c.universe._

  protected val baseRecordType = c.typeOf[Record]
  protected val baseFieldType = c.typeOf[Field[Any, Any]]

  protected def createFieldType(labelType: Type, valueType: Type): Type = {
    val TypeRef(fieldPre, fieldSym, _) = baseFieldType
    c.internal.typeRef(fieldPre, fieldSym, List(labelType, valueType))
  }

  protected def withType(acc: Option[Either[Tree, Type]], tpe: Type): Option[Either[Tree, Type]] =
    acc match {
      case None                      => Some(Right(tpe))
      case Some(Right(existingType)) => Some(Left(tq"$existingType with $tpe"))
      case Some(Left(existingType))  => Some(Left(tq"$existingType with $tpe"))
    }

  protected def toType(acc: Either[Tree, Type]): Type =
    acc match {
      case Right(tpe) => tpe
      case Left(tree) => c.typecheck(q"((???): $tree)").tpe
    }

  private def flattenRefinedTypes(tpe: c.Type): Seq[Type] =
    tpe match {
      case RefinedType(parents, _) => parents.flatMap(parent => flattenRefinedTypes(parent.dealias))
      case _                       => Seq(tpe)
    }

  protected def deconstructRecordType(recordType: Type): (Seq[(Type, Type)], Option[Type]) = {
    val (fields, rest) = flattenRefinedTypes(recordType.dealias)
      .foldLeft((List.empty[(Type, Type)], Option.empty[Either[Tree, Type]])) { case ((fields, rest), parentType) =>
        (if (parentType.typeSymbol == baseFieldType.typeSymbol) {
           parentType.typeArgs match {
             case List(labelType, valueType) => Some(labelType -> valueType)
             case _                          => None
           }
         } else {
           None
         }) match {
          case Some((labelType, valueType)) =>
            ((labelType -> valueType) :: fields, rest)
          case None =>
            (fields, withType(rest, parentType))
        }
      }
    (fields, rest.map(toType))
  }
}
