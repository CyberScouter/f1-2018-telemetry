import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import play.api.test._

/**
  * Add your integration spec here.
  * An integration test will fire up a whole play application in a real (or headless) browser.
  */
class IntegrationSpec
    extends PlaySpec
    with GuiceOneServerPerSuite
    with OneBrowserPerSuite
    with HtmlUnitFactory {

  "Application" should {
    "work from within a browser" in {

      go to s"http://localhost:$port"

      pageSource must include("shouts out")
    }
  }
}
