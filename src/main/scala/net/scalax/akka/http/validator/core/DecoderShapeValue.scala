package net.scalax.akka.http.validator.core

import akka.http.scaladsl.server._
import cats.data.Validated

trait DecoderShapeValue[Data] {

  type Rep
  def rep: Rep
  def shape: DecoderShape.Aux[Rep, Data, Rep]

}

object DecoderShapeValue {

  implicit def decoderShapeValueShapeImplicit[Data]: DecoderShape.Aux[DecoderShapeValue[Data], Data, DecoderShapeValue[Data]] = {
    new DecoderShape[DecoderShapeValue[Data], Data] {
      override type Target = DecoderShapeValue[Data]
      override def wrapRep(baseRep: DecoderShapeValue[Data]): DecoderShapeValue[Data] = baseRep
      override def toDirective(targetRep: DecoderShapeValue[Data]): Directive1[Validated[ErrorMessage, Data]] = targetRep.shape.toDirective(targetRep.rep)
    }
  }

}