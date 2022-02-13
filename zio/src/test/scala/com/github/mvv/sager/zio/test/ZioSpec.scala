package com.github.mvv.sager.zio.test

import com.github.mvv.sager.zio.FoundService
import com.github.mvv.sager.zio.blocking.Blocking
import com.github.mvv.sager.zio.clock.Clock
import com.github.mvv.sager.zio.console.Console
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers._

class ZioSpec extends AnyFunSpec {
  import ZioSpec._

  describe("ZIO service") {
    it("found in environment") {
      noException must be thrownBy implicitly[FoundService[Clock.Service, Environment]]
    }
  }
}

object ZioSpec {
  type Environment = Blocking with Clock with Console
}
