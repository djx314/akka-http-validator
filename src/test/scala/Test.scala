package net.scalax.akka.http.validator

import net.scalax.akka.http.validator.core.DecoderShapeValue
import shapeless._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._

trait Test extends App with AkkaHttpParameterHelper {

  case class RequestModel1(id: String, age: Int, name: String, account: String, num: Int)

  val pro = new DecoderProvenShape[RequestModel1] {
    val id = helper.parameter("id", "id")
    val age = helper.formField("age", "age".as[Int])
    val name = helper.formField("name", "name")
    val account = helper.param("account")
    val num = helper.form("num")

    override def sv: DecoderShapeValue[RequestModel1] = {
      fromModel.shaped(id :: age :: name :: account :: num :: HNil)
    }
  }

}