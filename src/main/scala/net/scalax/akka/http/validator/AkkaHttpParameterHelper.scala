package net.scalax.akka.http.validator

import akka.http.scaladsl.server._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.directives.FormFieldDirectives.{ FieldDef, FieldMagnet }
import akka.http.scaladsl.server.directives.ParameterDirectives.{ ParamDef, ParamMagnet }
import akka.http.scaladsl.unmarshalling._
import cats.data.Validated
import net.scalax.akka.http.validator.core.{ DecoderShape, ErrorMessage }
import net.scalax.akka.http.validator.helper.{ CommonHelper, HListDecoderImplicit, ParameterHelper, ParameterModel }
import shapeless.Witness

trait TotalHelper extends ParameterHelper with CommonHelper

object TotalHelper extends TotalHelper

trait AkkaHttpParameterHelper extends HListDecoderImplicit {

  val helper: TotalHelper = TotalHelper

  implicit def akkahttpParameterDValidatedShapeImplicit[K, T]: DecoderShape.Aux[ParameterModel.DValidated[T], T, ParameterModel.DValidated[T]] = new DecoderShape[ParameterModel.DValidated[T], T] {
    override type Target = ParameterModel.DValidated[T]
    override def wrapRep(baseRep: ParameterModel.DValidated[T]): ParameterModel.DValidated[T] = baseRep
    override def toDirective(targetRep: ParameterModel.DValidated[T]): Directive1[Validated[ErrorMessage, T]] = targetRep.directive
  }

  implicit def akkahttpParameterDirective1ShapeImplicit[T]: DecoderShape.Aux[Directive1[T], T, Directive1[T]] = new DecoderShape[Directive1[T], T] {
    override type Target = Directive1[T]
    override def wrapRep(baseRep: Directive1[T]): Directive1[T] = baseRep
    override def toDirective(targetRep: Directive1[T]): Directive1[Validated[ErrorMessage, T]] = targetRep.map(s => Validated.valid[ErrorMessage, T](s))
  }

  implicit def akkahttpParameterShapeImplicit[T](implicit fsu: FromStringUnmarshaller[T]): DecoderShape.Aux[ParameterModel.ParameterPlaceHolder, T, ParameterModel.DValidated[T]] = new DecoderShape[ParameterModel.ParameterPlaceHolder, T] {
    override type Target = ParameterModel.DValidated[T]
    override def wrapRep(baseRep: ParameterModel.ParameterPlaceHolder): ParameterModel.DValidated[T] = {
      val d1 = akka.http.scaladsl.server.Directives.parameter(ParamMagnet(baseRep.name.as[T])(ParamDef.forNR(fsu)))
      ParameterModel.dValidatedExtend(baseRep.name, d1)
    }
    override def toDirective(targetRep: ParameterModel.DValidated[T]): Directive1[Validated[ErrorMessage, T]] = targetRep.directive
  }

  implicit def akkahttpFormFieldShapeImplicit[T](implicit fsu: FromStrictFormFieldUnmarshaller[T]): DecoderShape.Aux[ParameterModel.FormFieldPlaceHolder, T, ParameterModel.DValidated[T]] = new DecoderShape[ParameterModel.FormFieldPlaceHolder, T] {
    override type Target = ParameterModel.DValidated[T]
    override def wrapRep(baseRep: ParameterModel.FormFieldPlaceHolder): ParameterModel.DValidated[T] = {
      val d1 = akka.http.scaladsl.server.Directives.formField(FieldMagnet(baseRep.name.as[T])(FieldDef.forNR(fsu)))
      ParameterModel.dValidatedExtend(baseRep.name, d1)
    }
    override def toDirective(targetRep: ParameterModel.DValidated[T]): Directive1[Validated[ErrorMessage, T]] = targetRep.directive
  }

  implicit def akkahttpOptionParameterShapeImplicit[T](implicit fsu: ParamDef.FSOU[T]): DecoderShape.Aux[ParameterModel.ParameterPlaceHolder, Option[T], ParameterModel.DValidated[Option[T]]] = new DecoderShape[ParameterModel.ParameterPlaceHolder, Option[T]] {
    override type Target = ParameterModel.DValidated[Option[T]]
    override def wrapRep(baseRep: ParameterModel.ParameterPlaceHolder): ParameterModel.DValidated[Option[T]] = {
      val d1 = akka.http.scaladsl.server.Directives.parameter(ParamMagnet(baseRep.name.as[T].?)(ParamDef.forNOR(fsu)))
      ParameterModel.dValidatedExtend(baseRep.name, d1)
    }
    override def toDirective(targetRep: ParameterModel.DValidated[Option[T]]): Directive1[Validated[ErrorMessage, Option[T]]] = targetRep.directive
  }

  implicit def akkahttpOptionFormFieldShapeImplicit[T](implicit fsu: FieldDef.FSFFOU[T]): DecoderShape.Aux[ParameterModel.FormFieldPlaceHolder, Option[T], ParameterModel.DValidated[Option[T]]] = new DecoderShape[ParameterModel.FormFieldPlaceHolder, Option[T]] {
    override type Target = ParameterModel.DValidated[Option[T]]
    override def wrapRep(baseRep: ParameterModel.FormFieldPlaceHolder): ParameterModel.DValidated[Option[T]] = {
      val d1 = akka.http.scaladsl.server.Directives.formField(FieldMagnet(baseRep.name.as[T].?)(FieldDef.forNOR(fsu)))
      ParameterModel.dValidatedExtend(baseRep.name, d1)
    }
    override def toDirective(targetRep: ParameterModel.DValidated[Option[T]]): Directive1[Validated[ErrorMessage, Option[T]]] = targetRep.directive
  }

}