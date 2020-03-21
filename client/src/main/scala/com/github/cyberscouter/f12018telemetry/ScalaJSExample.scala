package com.github.cyberscouter.f12018telemetry

import com.github.cyberscouter.f12018telemetry.shared.SharedMessages
import org.scalajs.dom

object ScalaJSExample {

  def main(args: Array[String]): Unit = {
    dom.document.getElementById("scalajsShoutOut").textContent = SharedMessages.itWorks
  }
}
