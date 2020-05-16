package com.github.mvv.sager.impl

import com.github.mvv.sager.Record

import scala.reflect.macros.blackbox

class SagerBlackBoxMacros(val c: blackbox.Context) extends SagerMacroUtils {
  import c.universe._

  def notFound[L: WeakTypeTag, R <: Record: WeakTypeTag]: Tree = {
    val labelType = weakTypeTag[L].tpe
    val recordType = weakTypeTag[R].tpe

    val (fields, rest) = deconstructRecordType(recordType)
    val dealiasedLabelType = labelType.dealias
    if (fields.contains(dealiasedLabelType)) {
      c.error(c.enclosingPosition, s"record $recordType contains a field with label $labelType")
    } else if (!isConcreteLabelType(dealiasedLabelType)) {
      fields.keys.headOption.foreach { otherLabelType =>
        c.error(c.enclosingPosition, s"could not prove that label $labelType is not equal to $otherLabelType")
      }
    } else {
      fields.keys.find(!isConcreteLabelType(_)).foreach { abstractLabelType =>
        c.error(c.enclosingPosition, s"could not prove that label $labelType is not equal to $abstractLabelType")
      }
    }
    if (rest.typeSymbol == baseRecordType.typeSymbol) {
      q"_root_.com.github.mvv.sager.Record.NotFound.unsafeMake[$labelType, $recordType]"
    } else {
      if (fields.isEmpty) {
        c.error(c.enclosingPosition,
                s"could not prove that a field with label $labelType is not a member of $recordType")
      }
      q"""
         {
           implicitly[_root_.com.github.mvv.sager.Record.NotFound[$labelType, $rest]]
           _root_.com.github.mvv.sager.Record.NotFound.unsafeMake[$labelType, $recordType]
         }
       """
    }
  }
}
