package com.github.mvv.sager.test

import com.github.mvv.sager.{Field, Record}
import com.github.mvv.sager.Record.Find
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers.*

class FindSpec extends AnyFunSpec:
  describe("Find") {
    it("derive for fields (present)") {
      """
         val find = summon[Find[Char, Nothing, Field[String, Int] & Field[Char, Float]]]
         summon[find.Rest =:= Field[String, Int]]
      """ must compile
    }

    it("derive for fields (absent)") {
      """
         val find = summon[Find[Int, Nothing, Field[String, Int] & Field[Char, Float]]]
         summon[find.Rest =:= (Field[String, Int] & Field[Char, Float])]
      """ must compile
    }
  }
