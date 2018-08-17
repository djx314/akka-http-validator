package net.scalax.akka.http.validator.helper

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.{ Directive, Directive1, Route }
import cats.data.Validated
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import net.scalax.akka.http.validator.core.{ DecoderShape, DecoderShapeValue, ErrorMessage }
import shapeless.Generic

trait CaseClassGen[Case, HListData] {

  val gen: Generic.Aux[Case, HListData]

  def shaped[HListRep](hlist: HListRep)(implicit shape: DecoderShape[HListRep, HListData]): DecoderShapeValue[Case] = {

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

object CaseClassGen

trait CommonHelper {

  import FailFastCirceSupport._
  import io.circe.syntax._

  def parallelValidate[A, B, C](v1: Validated[ErrorMessage, A], v2: Validated[ErrorMessage, B])(f: (A, B) => C): Validated[ErrorMessage, C] = {

    (v1, v2) match {
      case (Validated.Valid(a), Validated.Valid(b)) => Validated.Valid(f(a, b))
      case (Validated.Valid(_), i @ Validated.Invalid(_)) => i
      case (i @ Validated.Invalid(_), Validated.Valid(_)) => i
      case (Validated.Invalid(e1), Validated.Invalid(e2)) => Validated.Invalid(e1 ++: e2)
    }
  }

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
        complete((StatusCodes.BadRequest, message.asJson)): Directive1[Case]
    }
  }

}

object CommonHelper extends CommonHelper