package net.scalax.akka.http.validator.helper

import akka.http.scaladsl.model.{ HttpResponse, StatusCodes }
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.{ Directive, Directive1, Route }
import cats.data.Validated
import net.scalax.akka.http.validator.core.{ DecoderShape, DecoderShapeValue, ErrorMessage }
import shapeless.Generic

trait CaseClassGen[Case] {

  def apply[HListData, HListRep](hlist: HListRep)(implicit gen: Generic.Aux[Case, HListData], shape: DecoderShape[HListRep, HListData]): Directive1[Case] = {

    val d1: Directive1[Validated[ErrorMessage, HListData]] = shape.toDirective(shape.wrapRep(hlist))

    val d2 = d1.map(r => r.map(gen.from))

    CommonHelper.toResponse(d2)

  }

  def shaped[HListData, HListRep](hlist: HListRep)(implicit gen: Generic.Aux[Case, HListData], shape: DecoderShape[HListRep, HListData]): DecoderShapeValue[Case] = {
    val shape1 = shape
    new DecoderShapeValue[Case] {
      override type Rep = shape1.Target

      override def rep = shape1.wrapRep(hlist)

      override def shape: DecoderShape.Aux[shape1.Target, Case, shape1.Target] = new DecoderShape[shape1.Target, Case] {
        override type Target = shape1.Target
        override def wrapRep(baseRep: shape1.Target): shape1.Target = baseRep
        override def toDirective(targetRep: shape1.Target): Directive1[Validated[ErrorMessage, Case]] = shape1.toDirective(targetRep).map(_.map(gen.from))
      }
    }

  }

}

object CaseClassGen {
  def apply[Case]: CaseClassGen[Case] = new CaseClassGen[Case] {}
}

trait CommonHelper {

  def fromModel[Case]: CaseClassGen[Case] = CaseClassGen[Case]

  def fromShapeValue[Case](shapeValue: DecoderShapeValue[Case]): Directive1[Case] = {
    val sv = shapeValue
    val d1 = sv.shape.toDirective(sv.rep)
    toResponse(d1)
  }

  def toResponse[Case](d: Directive1[Validated[ErrorMessage, Case]]): Directive1[Case] = {
    d.flatMap {
      case Validated.Valid(d) =>
        Directive { (s: Tuple1[Case] => Route) => s(Tuple1(d)) }
      case Validated.Invalid(message) =>
        complete(HttpResponse(StatusCodes.BadRequest, entity = message.toString)): Directive1[Case]
    }
  }

}

object CommonHelper extends CommonHelper