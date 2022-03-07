package com.github.mvv.sager.test

import com.github.mvv.sager.{Field, Record}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers.*

class RecordSpec extends AnyFunSpec:
  describe("Record") {
    it("add non-existent field") {
      val r = Field[Int]("a").add[Long](true).add[String](0.0)
      (r.get[Int]: String) mustEqual "a"
      (r.get[Long]: Boolean) mustEqual true
      (r.get[String]: Double) mustEqual 0.0
    }

    it("override existing field") {
      val r = Field[Int]("a")
      val r1 = r.add[Int](true)
      (r1.get[Int]: Boolean) mustEqual true
      val r2 = r.add[Long](true).add[Int](0.0f)
      (r2: Field[Long, Boolean] with Field[Int, Float]) mustEqual r2
      (r2.get[Long]: Boolean) mustEqual true
      (r2.get[Int]: Float) mustEqual 0.0f
    }

    it("updating field") {
      Field[Int]("a").add[Long](true).update[Int]((_: String) + "b").get[Int] mustEqual "ab"
    }

    it("updating field via map") {
      Field[Int]("a").add[Long](true).field[Int].map(_ + "b").get[Int] mustEqual "ab"
    }
  }
