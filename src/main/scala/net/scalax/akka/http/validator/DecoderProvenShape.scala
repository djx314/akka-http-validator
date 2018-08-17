package net.scalax.akka.http.validator

import akka.http.scaladsl.server._
import cats.data.Validated
import net.scalax.akka.http.validator.core.{ DecoderShape, DecoderShapeValue, ErrorMessage }
import net.scalax.akka.http.validator.helper.{ CaseClassGen, CommonHelper }

trait DecoderProvenShape[Data] {
  self =>

  val fromRep: CaseClassGen[Data] = CaseClassGen[Data]

  def sv: DecoderShapeValue[Data]

  def toDirective1: Directive1[Data] = CommonHelper.fromShapeValue(self.sv)

}

object DecoderProvenShape {

  implicit def decoderProvenShapeShapeImplicit[Data]: DecoderShape.Aux[DecoderProvenShape[Data], Data, DecoderProvenShape[Data]] = {
    new DecoderShape[DecoderProvenShape[Data], Data] {
      override type Target = DecoderProvenShape[Data]
      override def wrapRep(baseRep: DecoderProvenShape[Data]): DecoderProvenShape[Data] = baseRep
      override def toDirective(targetRep: DecoderProvenShape[Data]): Directive1[Validated[ErrorMessage, Data]] = {
        val svImplicit = implicitly[DecoderShape.Aux[DecoderShapeValue[Data], Data, DecoderShapeValue[Data]]]
        svImplicit.toDirective(targetRep.sv)
      }
    }
  }

}