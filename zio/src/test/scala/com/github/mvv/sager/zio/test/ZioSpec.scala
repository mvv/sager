package com.github.mvv.sager.zio.test

import com.github.mvv.sager.zio.FoundService
import com.github.mvv.sager.zio.blocking.Blocking
import com.github.mvv.sager.zio.clock.Clock
import com.github.mvv.sager.zio.console.Console
import org.specs2.mutable.Specification

class ZioSpec extends Specification {
  import ZioSpec._

  "ZIO service" >> {
    "found in environment" >> {
      implicitly[FoundService[Clock.Service, Environment]] must not(throwA[Throwable])
    }
  }
}

object ZioSpec {
  type Environment = Blocking with Clock with Console
}
