package net.scalax.akka.http.validator

import akka.http.scaladsl.server._
import cats.data.Validated
import net.scalax.akka.http.validator.core.{ DecoderShape, DecoderShapeValue, ErrorMessage }
import net.scalax.akka.http.validator.helper.{ CaseClassGen, CommonHelper }
import shapeless.Generic

trait DecoderProvenShape[Data] {
  self =>

  def fromModel[HListData](implicit gen: Generic.Aux[Data, HListData]): CaseClassGen[Data, HListData] = {
    val gen1 = gen
    new CaseClassGen[Data, HListData] {
      override val gen = gen1
    }
  }

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