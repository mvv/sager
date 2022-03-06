package com.github.mvv.sager.impl

import com.github.mvv.sager.Record
import com.github.mvv.typine.impl.TypineMacros

import scala.reflect.macros.blackbox

class SagerBlackBoxMacros(val c: blackbox.Context) extends SagerMacroUtils {
  import c.universe._

  def notFound[L: WeakTypeTag, R <: Record: WeakTypeTag]: Tree = {
    val labelType = weakTypeTag[L].tpe
    val recordType = weakTypeTag[R].tpe

    val (fields, rest) = deconstructRecordType(recordType)
    val (diffType, unknownType) =
      fields.foldLeft((Option.empty[Either[Tree, Type]], rest.map(Right(_): Either[Tree, Type]))) {
        case ((diffType, unknownType), (fieldLabelType, fieldValueType)) =>
          val fieldType = createFieldType(fieldLabelType, fieldValueType)
          if (TypineMacros.searchUnequal(c)(labelType, fieldLabelType) != EmptyTree) {
            (withType(diffType, fieldType), unknownType)
          } else {
            (diffType, withType(unknownType, fieldType))
          }
      }
    diffType.map(toType) match {
      case Some(diffType) =>
        val absentInDiff =
          q"_root_.com.github.mvv.sager.Record.NotFound.unsafeMake[$labelType, $diffType]"
        unknownType.map(toType) match {
          case Some(unknownType) =>
            q"""
               _root_.com.github.mvv.sager.Record.NotFound.make[$labelType, $diffType, $unknownType](
                 $absentInDiff,
                 implicitly[_root_.com.github.mvv.sager.Record.NotFound[$labelType, $unknownType]])
             """
          case None =>
            absentInDiff
        }
      case None =>
        EmptyTree
    }
  }
}
