package com.github.mvv.sager.impl

import com.github.mvv.sager.Record

import scala.reflect.macros.whitebox

class SagerWhiteBoxMacros(val c: whitebox.Context) extends SagerMacroUtils {
  import c.universe._

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
    q"_root_.com.github.mvv.sager.Record.Select.make[$labelType, $valueType, $recordType]"
  }

  def found[L: WeakTypeTag, R <: Record: WeakTypeTag](witness: Tree): Tree = {
    val labelType = weakTypeTag[L].tpe
    val recordType = weakTypeTag[R].tpe
    val (fields, rest0) = deconstructRecordType(recordType)
    val dealiasedLabelType = labelType.dealias
    fields.get(dealiasedLabelType) match {
      case Some(valueType) =>
        val restTree = fields.foldLeft(Right(rest0): Either[Tree, Type]) {
          case (rest, (fieldLabelType, _)) if fieldLabelType == labelType =>
            rest
          case (rest, (fieldLabelType, fieldValueType)) =>
            rest match {
              case Right(restType) =>
                Left(tq"$restType with _root_.com.github.mvv.sager.Field[$fieldLabelType, $fieldValueType]")
              case Left(restTree) =>
                Left(tq"$restTree with _root_.com.github.mvv.sager.Field[$fieldLabelType, $fieldValueType]")
            }
        }
        val restType = restTree.fold[Type](c.typecheck(_).tpe, identity[Type])
        q"_root_.com.github.mvv.sager.Record.FoundValue.make[$labelType, $valueType, $recordType, $restType]"
      case None =>
        c.error(c.enclosingPosition, s"could not find a field with label $labelType in record $recordType")
        EmptyTree
    }
  }
}
