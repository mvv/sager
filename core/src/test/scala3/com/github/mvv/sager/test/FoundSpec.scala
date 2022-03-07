package com.github.mvv.sager.test

import com.github.mvv.sager.{Field, Record}
import com.github.mvv.sager.Record.{Absent, Found, FoundSome}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers.*

class FoundSpec extends AnyFunSpec:
  describe("Found") {
    it("derive for fields") {
      """
         val found = summon[FoundSome[Char, Field[String, Int] & Field[Char, Float]]]
         summon[found.Value =:= Float]
         summon[found.Rest =:= Field[String, Int]]
      """ must compile
    }

    it("derive for fields + absent") {
      """
         def f[R <: Record](using Absent[Char, R]) =
           val found = summon[FoundSome[Char, Field[String, Int] & Field[Char, Float] & R]]
           summon[found.Value =:= Float]
           summon[found.Rest =:= (Field[String, Int] & R)]
      """ must compile
    }

    it("derive for fields + present") {
      """
         def f[R <: Record](using w: FoundSome[Char, R]) =
           val found = summon[FoundSome[Char, Field[String, Int] & R]]
           summon[found.Value =:= w.Value]
           summon[found.Rest =:= (w.Rest & Field[String, Int])]
      """ must compile
    }
  }
