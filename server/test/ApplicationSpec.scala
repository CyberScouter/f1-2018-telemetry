import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import play.api.test._
import play.api.test.Helpers._

/**
  * Add your spec here.
  * You can mock out a whole application including requests, plugins etc.
  * For more information, consult the wiki.
  */
class ApplicationSpec extends PlaySpec with GuiceOneAppPerTest {

  "Application" should {

    "send 404 on a bad request" in {
      val Some(result) = route(app, FakeRequest(GET, "/boum"))
      status(result) mustBe NOT_FOUND
    }

    "render the index page" in {
      val home = route(app, FakeRequest(GET, "/")).get

      status(home) mustBe OK
      contentType(home) mustBe Some("text/html")
      contentAsString(home) must include("shouts out")
    }
  }
}
