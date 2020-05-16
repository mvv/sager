package com.github.mvv.sager.test

import com.github.mvv.sager.{Field, Record}
import org.specs2.matcher.TypecheckMatchers
import org.specs2.mutable.Specification

class RecordSpec extends Specification with TypecheckMatchers {
  "Record" >> {
    "add non-existent field" >> {
      val r = Field[Int]("a").add[Long](true).add[String](0.0)
      (r.get[Int]: String) mustEqual "a"
      (r.get[Long]: Boolean) mustEqual true
      (r.get[String]: Double) mustEqual 0.0
    }

    "override existing field" >> {
      val r = Field[Int]("a")
      val r1 = r.add[Int](true)
      (r1.get[Int]: Boolean) mustEqual true
      val r2 = r.add[Long](true).add[Int](0.0)
      (r2.get[Long]: Boolean) mustEqual true
      (r2.get[Int]: Double) mustEqual 0.0
    }

    "updating field" >> {
      Field[Int]("a").add[Long](true).update[Int]((_: String) + "b").get[Int] mustEqual "ab"
    }

    "updating field via map" >> {
      Field[Int]("a").add[Long](true).field[Int].map(_ + "b").get[Int] mustEqual "ab"
    }
  }
}
