package net.scalax.akka.http.validator

import akka.Done
import akka.http.scaladsl.model._
import net.scalax.akka.http.validator.core.{ DecoderShapeValue, ErrorPath }
import shapeless._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import cats.data.Validated
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import org.scalatest.{ Matchers, WordSpec }
import io.circe.syntax._
import io.circe.generic.auto._

case class RequestModel1(id: String, age: Int, name: String, account: String, num: Int)

object CommonRoute extends AkkaHttpParameterHelper with FailFastCirceSupport {

  import scala.concurrent.ExecutionContext.Implicits.global

  lazy val pro = new DecoderProvenShape[RequestModel1] {

    val id = helper.parameter("id")

    val age = helper.formField("age".as[Int]).transform { g =>
      if (g > 34346345)
        Validated.invalid(path.resolve("age").toMessage("年龄不可以大于 34346345 岁"))
      else
        Validated.valid(g + 10000000)
    }

    val name = helper.formField("name").validate { g =>
      if (!g.startsWith("name"))
        Validated.invalid(path.resolve("name").resolve("prefix").toMessage("名字必须以 name 开头"))
      else
        Validated.valid(Done)
    }

    val account = helper.param("account")
    val num = helper.form("num")

    override def sv: DecoderShapeValue[RequestModel1] = {
      fromModel.shaped(id :: age :: name :: account :: num :: HNil)
    }

  }

  def route = path("ping") {
    post {
      pro.toDirective1 { model =>
        complete(model.asJson)
      }
    }
  }
}

class FullTestKitExampleSpec extends WordSpec with Matchers with ScalatestRouteTest {

  lazy val smallRoute = CommonRoute.route

  "The service" should {

    "return a ok reponse when send post method" in {
      Post("/ping?id=id1&account=account1", FormData(("age", "34346345"), ("name", "name1"), ("num", "23484"))) ~> Route.seal(smallRoute) ~> check {
        status shouldEqual StatusCodes.OK
        io.circe.parser.parse(responseAs[String]).right.get shouldEqual RequestModel1(id = "id1", age = 44346345, name = "name1", account = "account1", num = 23484).asJson
      }
    }

    "return a bad response for post large age" in {
      Post("/ping?id=id1&account=account1", FormData(("age", "34349727"), ("name", "myName1"), ("num", "23484"))) ~> Route.seal(smallRoute) ~> check {
        status shouldEqual StatusCodes.BadRequest

        io.circe.parser.parse(responseAs[String]).right.get shouldEqual
          io.circe.parser.parse(
            """
              {
                "age": ["年龄不可以大于 34346345 岁"] ,
                "name.prefix": ["名字必须以 name 开头"]
              }
            """.stripMargin).right.get

        io.circe.parser.parse(responseAs[String]).right.get shouldEqual (ErrorPath.empty.resolve("age").toMessage("年龄不可以大于 34346345 岁") ++:
          ErrorPath.empty.resolve("name.prefix").toMessage("名字必须以 name 开头")).asJson
      }
    }

  }
}