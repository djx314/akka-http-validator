package net.scalax.akka.http.validator

import akka.http.scaladsl.server._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.directives.FormFieldDirectives.{ FieldDef, FieldMagnet }
import akka.http.scaladsl.server.directives.ParameterDirectives.{ ParamDef, ParamMagnet }
import akka.http.scaladsl.unmarshalling._
import cats.data.Validated
import net.scalax.akka.http.validator.core.{ DecoderShape, ErrorMessage }
import net.scalax.akka.http.validator.helper._

import scala.concurrent.ExecutionContext

trait TotalHelper extends ParameterHelper with CommonHelper

object TotalHelper extends TotalHelper

trait AkkaHttpParameterHelper extends HListDecoderImplicit {

  val helper: TotalHelper = TotalHelper

  implicit def akkahttpParameterDValidatedShapeImplicit[K, T](implicit ec: ExecutionContext): DecoderShape.Aux[DValidated[T], T, DValidated[T]] = new DecoderShape[DValidated[T], T] {
    override type Target = DValidated[T]
    override def wrapRep(baseRep: DValidated[T]): DValidated[T] = baseRep
    override def toDirective(targetRep: DValidated[T]): Directive1[Validated[ErrorMessage, T]] = targetRep.directive
  }

  implicit def akkahttpParameterDirective1ShapeImplicit[T](implicit ec: ExecutionContext): DecoderShape.Aux[Directive1[T], T, Directive1[T]] = new DecoderShape[Directive1[T], T] {
    override type Target = Directive1[T]
    override def wrapRep(baseRep: Directive1[T]): Directive1[T] = baseRep
    override def toDirective(targetRep: Directive1[T]): Directive1[Validated[ErrorMessage, T]] = DValidated.fromDirective(targetRep).directive
  }

  implicit def akkahttpParameterShapeImplicit[T](implicit fsu: FromStringUnmarshaller[T], ec: ExecutionContext): DecoderShape.Aux[ParameterModel.ParameterPlaceHolder, T, DValidated[T]] = new DecoderShape[ParameterModel.ParameterPlaceHolder, T] {
    override type Target = DValidated[T]
    override def wrapRep(baseRep: ParameterModel.ParameterPlaceHolder): DValidated[T] = {
      val d1 = akka.http.scaladsl.server.Directives.parameter(ParamMagnet(baseRep.name.as[T])(ParamDef.forNR(fsu)))
      DValidated.fromDirective(d1)
    }
    override def toDirective(targetRep: DValidated[T]): Directive1[Validated[ErrorMessage, T]] = targetRep.directive
  }

  implicit def akkahttpFormFieldShapeImplicit[T](implicit fsu: FromStrictFormFieldUnmarshaller[T], ec: ExecutionContext): DecoderShape.Aux[ParameterModel.FormFieldPlaceHolder, T, DValidated[T]] = new DecoderShape[ParameterModel.FormFieldPlaceHolder, T] {
    override type Target = DValidated[T]
    override def wrapRep(baseRep: ParameterModel.FormFieldPlaceHolder): DValidated[T] = {
      val d1 = akka.http.scaladsl.server.Directives.formField(FieldMagnet(baseRep.name.as[T])(FieldDef.forNR(fsu)))
      DValidated.fromDirective(d1)
    }
    override def toDirective(targetRep: DValidated[T]): Directive1[Validated[ErrorMessage, T]] = targetRep.directive
  }

  implicit def akkahttpOptionParameterShapeImplicit[T](implicit fsu: ParamDef.FSOU[T], ec: ExecutionContext): DecoderShape.Aux[ParameterModel.ParameterPlaceHolder, Option[T], DValidated[Option[T]]] = new DecoderShape[ParameterModel.ParameterPlaceHolder, Option[T]] {
    override type Target = DValidated[Option[T]]
    override def wrapRep(baseRep: ParameterModel.ParameterPlaceHolder): DValidated[Option[T]] = {
      val d1 = akka.http.scaladsl.server.Directives.parameter(ParamMagnet(baseRep.name.as[T].?)(ParamDef.forNOR(fsu)))
      DValidated.fromDirective(d1)
    }
    override def toDirective(targetRep: DValidated[Option[T]]): Directive1[Validated[ErrorMessage, Option[T]]] = targetRep.directive
  }

  implicit def akkahttpOptionFormFieldShapeImplicit[T](implicit fsu: FieldDef.FSFFOU[T], ec: ExecutionContext): DecoderShape.Aux[ParameterModel.FormFieldPlaceHolder, Option[T], DValidated[Option[T]]] = new DecoderShape[ParameterModel.FormFieldPlaceHolder, Option[T]] {
    override type Target = DValidated[Option[T]]
    override def wrapRep(baseRep: ParameterModel.FormFieldPlaceHolder): DValidated[Option[T]] = {
      val d1 = akka.http.scaladsl.server.Directives.formField(FieldMagnet(baseRep.name.as[T].?)(FieldDef.forNOR(fsu)))
      DValidated.fromDirective(d1)
    }
    override def toDirective(targetRep: DValidated[Option[T]]): Directive1[Validated[ErrorMessage, Option[T]]] = targetRep.directive
  }

}