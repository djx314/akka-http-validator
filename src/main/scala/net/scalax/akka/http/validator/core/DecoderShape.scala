package net.scalax.akka.http.validator.core

import akka.http.scaladsl.server._
import cats.data.Validated
import io.circe.{ Encoder, KeyEncoder }
import io.circe.syntax._

case class ErrorMessage(msg: List[(ErrorPath, String)]) {
  self =>

  def ++:(other: ErrorMessage): ErrorMessage = {
    ErrorMessage(self.msg ++: other.msg)
  }

}

object ErrorMessage {
  implicit val encoder: Encoder[ErrorMessage] = Encoder.instance { s =>
    val jsonModel = s.msg.groupBy(_._1).map { case (errPath, value) => (errPath, value.map(_._2)) }
    implicit val keyEncoder = KeyEncoder.encodeKeyString.contramap { (key: ErrorPath) => key.toKey }
    jsonModel.asJson
  }
}

class ErrorPath private (lawPaths: List[String]) {
  self =>

  def resolve(path: String): ErrorPath = new ErrorPath(path :: lawPaths)
  def paths: List[String] = lawPaths.reverse
  def toKey: String = paths.mkString(".")

  def toMessage(value: String): ErrorMessage = {
    ErrorMessage(msg = List((self, value)))
  }

  def toMessages(values: List[String]): ErrorMessage = {
    ErrorMessage(msg = values.map(value => (self, value)))
  }

}

object ErrorPath {
  val empty: ErrorPath = new ErrorPath(List.empty)
  def init(name: String): ErrorPath = new ErrorPath(name :: Nil)
  def fromList(paths: List[String]): ErrorPath = new ErrorPath(paths.reverse)
}

/*trait SingleMessage {

  val key: String

  def toMessage(msgs: List[String]): ErrorMessage = ??? //ErrorMessage(msgs.map(m => (key, m)))
  def toMessage(msg: String): ErrorMessage = ??? //ErrorMessage(List((key, msg)))

}

object SingleMessage {
  def toMessage(msgs: List[String]): SingleMessage => ErrorMessage = { s => s.toMessage(msgs) }
  def toMessage(msg: String): SingleMessage => ErrorMessage = { s => s.toMessage(msg) }
}

case class SingleMessageImpl(override val key: String) extends SingleMessage*/

trait DecoderShape[-Rep, Data] {
  self =>

  type Target

  def wrapRep(baseRep: Rep): Target
  def toDirective(targetRep: Target): Directive1[Validated[ErrorMessage, Data]]

  def packed: DecoderShape.Aux[Target, Data, Target] = new DecoderShape[Target, Data] {
    override type Target = self.Target
    override def wrapRep(baseRep: self.Target): self.Target = baseRep
    override def toDirective(targetRep: Target): Directive1[Validated[ErrorMessage, Data]] = self.toDirective(targetRep)
  }

}

object DecoderShape {

  type Aux[-Rep, Data, Target1] = DecoderShape[Rep, Data] { type Target = Target1 }

}