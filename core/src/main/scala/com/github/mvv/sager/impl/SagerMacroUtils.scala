package com.github.mvv.sager.impl

import com.github.mvv.sager.{Field, Record}

import scala.reflect.macros.blackbox

trait SagerMacroUtils {
  val c: blackbox.Context
  import c.universe._

  protected val baseRecordType = c.typeOf[Record]
  protected val baseFieldType = c.typeOf[Field[_, Any]]

  private def flattenRefinedTypes(tpe: c.Type): Seq[Type] =
    tpe match {
      case RefinedType(parents, _) => parents.flatMap(parent => flattenRefinedTypes(parent.dealias))
      case _                       => Seq(tpe)
    }

  protected def deconstructRecordType(recordType: Type): (Map[Type, Type], Type) = {
    val (fields, rest) = flattenRefinedTypes(recordType.dealias)
      .foldLeft((Map.empty[Type, Either[Tree, Type]], Right(baseRecordType): Either[Tree, Type])) {
        case ((fields, rest), parentType) =>
          (if (parentType.typeSymbol == baseFieldType.typeSymbol) {
             parentType.typeArgs match {
               case List(labelType, valueType) => Some(labelType -> valueType)
               case _                          => None
             }
           } else {
             None
           }) match {
            case Some((labelType, valueType)) =>
              val updatedType = fields.get(labelType) match {
                case None                      => Right(valueType)
                case Some(Right(existingType)) => Left(tq"$existingType with $valueType")
                case Some(Left(existingTree))  => Left(tq"$existingTree with $valueType")
              }
              (fields + (labelType -> updatedType), rest)
            case None =>
              val updatedRest = rest match {
                case Right(existingType) if existingType == baseRecordType =>
                  Right(parentType)
                case Right(existingType) =>
                  Left(tq"$existingType with $parentType")
                case Left(existingTree) =>
                  Left(tq"$existingTree with $parentType")
              }
              (fields, updatedRest)
          }
      }
    (fields.map {
       case (labelType, Right(valueType)) => (labelType, valueType)
       case (labelType, Left(valueTree))  => (labelType, c.typecheck(q"((???): $valueTree)").tpe)
     },
     rest match {
       case Right(restType) => restType
       case Left(restTree)  => c.typecheck(q"((???): $restTree)").tpe
     })
  }

  protected def isConcreteLabelType(labelType: Type): Boolean =
    labelType.typeSymbol.isClass && labelType.typeArgs.forall(isConcreteLabelType)
}
