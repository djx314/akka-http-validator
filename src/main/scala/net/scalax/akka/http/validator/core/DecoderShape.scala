package net.scalax.akka.http.validator.core

import akka.http.scaladsl.server._
import cats.data.Validated

case class ErrorMessage(msg: List[(String, String)])

trait SingleMessage {

  val key: String

  def toMessage(msgs: List[String]): ErrorMessage = ErrorMessage(msgs.map(m => (key, m)))
  def toMessage(msg: String): ErrorMessage = ErrorMessage(List((key, msg)))

}

object SingleMessage {
  def toMessage(msgs: List[String]): SingleMessage => ErrorMessage = { s => s.toMessage(msgs) }
  def toMessage(msg: String): SingleMessage => ErrorMessage = { s => s.toMessage(msg) }
}

case class SingleMessageImpl(override val key: String) extends SingleMessage

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