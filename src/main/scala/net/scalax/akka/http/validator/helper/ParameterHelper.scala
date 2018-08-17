package net.scalax.akka.http.validator.helper

import akka.http.scaladsl.server._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.directives.FormFieldDirectives.FieldMagnet
import akka.http.scaladsl.server.directives.ParameterDirectives.ParamMagnet
import cats.data.Validated
import net.scalax.akka.http.validator.core.{ ErrorMessage, SingleMessage, SingleMessageImpl }

import scala.concurrent.{ ExecutionContext, Future }

object ParameterModel {
  trait DValidated[T] {
    self =>

    val key: String

    val directive: Directive1[Validated[ErrorMessage, T]]

    def map[R](s: T => R): DValidated[R] = new DValidated[R] {
      override val key = self.key
      override val directive: Directive1[Validated[ErrorMessage, R]] = self.directive.map(_.map(s))
    }

    def validate[R](s: T => Validated[SingleMessage => ErrorMessage, R]): DValidated[R] = new DValidated[R] {
      override val key = self.key
      override val directive: Directive1[Validated[ErrorMessage, R]] = self.directive.map(_.andThen(t => s(t).leftMap(m => m(SingleMessageImpl(key)))))
    }

    def fmap[R](s: T => Future[R])(implicit ec: ExecutionContext): DValidated[R] = new DValidated[R] {
      override val key = self.key
      override val directive: Directive1[Validated[ErrorMessage, R]] = self.directive.flatMap { r =>
        val f = r.map(s) match {
          case Validated.Valid(u) =>
            u.map(v => Validated.valid(v): Validated[ErrorMessage, R])
          case Validated.Invalid(err) =>
            Future.successful(Validated.Invalid(err): Validated[ErrorMessage, R])

        }
        onSuccess(f)
      }
    }

    def fvalidate[R](s: T => Future[Validated[SingleMessage => ErrorMessage, R]])(implicit ec: ExecutionContext): DValidated[R] = {
      self.fmap(s).validate(identity)
    }
  }

  trait ParameterPlaceHolder {
    val name: String
  }

  object ParameterPlaceHolder {
    def apply(n: String): ParameterPlaceHolder = new ParameterPlaceHolder {
      override val name = n
    }
  }

  trait FormFieldPlaceHolder {
    val name: String
  }

  object FormFieldPlaceHolder {
    def apply(n: String): FormFieldPlaceHolder = new FormFieldPlaceHolder {
      override val name = n
    }
  }

  def dValidatedExtend[T](name: String, validated: Directive1[T]): ParameterModel.DValidated[T] = {
    new ParameterModel.DValidated[T] {
      override val key = name
      override val directive: Directive1[Validated[ErrorMessage, T]] = validated.map(s => Validated.Valid(s): Validated[ErrorMessage, T])
    }
  }

}

trait ParameterHelper {

  type ParamMagnetAux[U] = ParamMagnet { type Out = U }
  type FieldMagnetAux[U] = FieldMagnet { type Out = U }

  def parameter[T](name: String, pdm: ParamMagnetAux[Directive1[T]]): ParameterModel.DValidated[T] = {
    val d1 = akka.http.scaladsl.server.Directives.parameter(pdm)
    ParameterModel.dValidatedExtend(name, d1)
  }

  def formField[T](name: String, pdm: FieldMagnetAux[Directive1[T]]): ParameterModel.DValidated[T] = {
    val d1 = akka.http.scaladsl.server.Directives.formField(pdm)
    ParameterModel.dValidatedExtend(name, d1)
  }

  def simpleParameter(name: String): ParameterModel.DValidated[String] = {
    val d1 = akka.http.scaladsl.server.Directives.parameter(name)
    ParameterModel.dValidatedExtend(name, d1)
  }

  def simpleFormField(name: String): ParameterModel.DValidated[String] = {
    val d1 = akka.http.scaladsl.server.Directives.formField(name)
    ParameterModel.dValidatedExtend(name, d1)
  }

  def simpleParameterOpt(name: String): ParameterModel.DValidated[Option[String]] = {
    val d1 = akka.http.scaladsl.server.Directives.parameter(name.as[String].?)
    ParameterModel.dValidatedExtend(name, d1)
  }

  def simpleFormFieldOpt(name: String): ParameterModel.DValidated[Option[String]] = {
    val d1 = akka.http.scaladsl.server.Directives.formField(name.as[String].?)
    ParameterModel.dValidatedExtend(name, d1)
  }

  /*def parameter[T](name: String)(implicit fsu: FromStringUnmarshaller[T]): ParameterModel.DValidated[T] = {
    val d1 = akka.http.scaladsl.server.Directives.parameter(ParamMagnet(name.as[T])(ParamDef.forNR(fsu)))
    ParameterModel.dValidatedExtend(name, d1)
  }

  def parameterOpt[T](name: String)(implicit fsu: ParamDef.FSOU[T]): ParameterModel.DValidated[Option[T]] = {
    val d1 = akka.http.scaladsl.server.Directives.parameter(ParamMagnet(name.as[T].?)(ParamDef.forNOR(fsu)))
    ParameterModel.dValidatedExtend(name, d1)
  }

  def formField[T](name: String)(implicit fsu: FromStrictFormFieldUnmarshaller[T]): ParameterModel.DValidated[T] = {
    val d1 = akka.http.scaladsl.server.Directives.formField(FieldMagnet(name.as[T])(FieldDef.forNR(fsu)))
    ParameterModel.dValidatedExtend(name, d1)
  }

  def formFieldOpt[T](name: String)(implicit fsu: FieldDef.FSFFOU[T]): ParameterModel.DValidated[Option[T]] = {
    val d1 = akka.http.scaladsl.server.Directives.formField(FieldMagnet(name.as[T].?)(FieldDef.forNOR(fsu)))
    ParameterModel.dValidatedExtend(name, d1)
  }*/

  def param(name: String): ParameterModel.ParameterPlaceHolder = ParameterModel.ParameterPlaceHolder(name)
  def form(name: String): ParameterModel.FormFieldPlaceHolder = ParameterModel.FormFieldPlaceHolder(name)

}

object ParameterHelper extends ParameterHelper