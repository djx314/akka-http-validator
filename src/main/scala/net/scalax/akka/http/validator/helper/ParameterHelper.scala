package net.scalax.akka.http.validator.helper

import akka.Done
import akka.http.scaladsl.server._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.directives.FormFieldDirectives.FieldMagnet
import akka.http.scaladsl.server.directives.ParameterDirectives.ParamMagnet
import cats.data.Validated
import net.scalax.akka.http.validator.core.{ ErrorMessage, ErrorPath }

import scala.concurrent.{ ExecutionContext, Future }

trait DValidated[T] {
  self =>

  protected val baseDirective: Directive1[Validated[ErrorMessage, T]]
  protected val validators: List[T => Future[Validated[ErrorMessage, Done]]]

  def directive(implicit ec: ExecutionContext): Directive1[Validated[ErrorMessage, T]] = {
    baseDirective.flatMap { d =>
      d match {
        case Validated.Valid(data) =>
          val resultF = Future.sequence(validators.map(vf => vf(data))).map { results =>
            val errMsgs = results.collect { case Validated.Invalid(err) => err }
            if (errMsgs.isEmpty) {
              baseDirective
            } else {
              val errMsg = errMsgs.reduce(_ ++: _)
              Directive((s: Tuple1[Validated[ErrorMessage, T]] => Route) => s(Tuple1(Validated.invalid(errMsg))))
            }
          }
          onSuccess(resultF).flatMap(identity)
        case Validated.Invalid(err) =>
          Directive((s: Tuple1[Validated[ErrorMessage, T]] => Route) => s(Tuple1(Validated.invalid(err))))
      }
    }
  }

  def map[R](s: T => R)(implicit ec: ExecutionContext): DValidated[R] = new DValidated[R] {
    override val baseDirective: Directive1[Validated[ErrorMessage, R]] = self.directive.map(_.map(s))
    override val validators = List.empty
  }

  def flatMap[R](s: T => DValidated[R])(implicit ec: ExecutionContext): DValidated[R] = new DValidated[R] {
    override val baseDirective: Directive1[Validated[ErrorMessage, R]] = self.directive.flatMap {
      case Validated.Valid(succ) =>
        s(succ).directive.map(t => t: Validated[ErrorMessage, R])
      case Validated.Invalid(err) =>
        Directive((s: Tuple1[Validated[ErrorMessage, R]] => Route) => s(Tuple1(Validated.Invalid(err): Validated[ErrorMessage, R])))
    }
    override val validators = List.empty
  }

  def transform[R](s: T => Validated[ErrorMessage, R])(implicit ec: ExecutionContext): DValidated[R] = new DValidated[R] {
    override val baseDirective: Directive1[Validated[ErrorMessage, R]] = self.directive.map(_.andThen(t => s(t)))
    override val validators = List.empty
  }

  def fmap[R](s: T => Future[R])(implicit ec: ExecutionContext): DValidated[R] = new DValidated[R] {
    override val baseDirective: Directive1[Validated[ErrorMessage, R]] = self.directive.flatMap { r =>
      val f = r.map(s) match {
        case Validated.Valid(u) =>
          u.map(v => Validated.valid(v): Validated[ErrorMessage, R])
        case Validated.Invalid(err) =>
          Future.successful(Validated.Invalid(err): Validated[ErrorMessage, R])

      }
      onSuccess(f)
    }
    override val validators = List.empty
  }

  def ftransform[R](s: T => Future[Validated[ErrorMessage, R]])(implicit ec: ExecutionContext): DValidated[R] = {
    self.fmap(s).transform(identity)
  }

  def validate(s: T => Validated[ErrorMessage, Done])(implicit ec: ExecutionContext): DValidated[T] = new DValidated[T] {
    override val baseDirective = self.baseDirective
    override val validators = List(s.andThen(Future.successful))
  }

  def fvalidate(s: T => Future[Validated[ErrorMessage, Done]])(implicit ec: ExecutionContext): DValidated[T] = new DValidated[T] {
    override val baseDirective = self.baseDirective
    override val validators = List(s)
  }

}

object DValidated {
  def sequence[T](parentPath: ErrorPath, f: List[ErrorPath => DValidated[T]])(implicit ec: ExecutionContext): DValidated[List[T]] = {
    val validateList = f.zipWithIndex.map {
      case (i, index) =>
        i(parentPath.resolve(index.toString))
    }

    val emptyListD: DValidated[List[T]] = apply(List.empty)

    validateList.foldLeft(emptyListD) {
      case (result, item) =>
        result.flatMap(r => item.map(t => t :: r))
    }
  }

  def apply[T](t: T): DValidated[T] = new DValidated[T] {
    override val baseDirective = Directive((s: Tuple1[Validated[ErrorMessage, T]] => Route) => s(Tuple1(Validated.valid(t))))
    override val validators = List.empty
  }

  def fromDirective[T](d: Directive1[T]): DValidated[T] = {
    new DValidated[T] {
      override val baseDirective = d.map(s => Validated.valid(s): Validated[ErrorMessage, T])
      override val validators = List.empty
    }
  }
}

object ParameterModel {

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

}

trait ParameterHelper {

  def parameter[T](pdm: ParamMagnet)(implicit cv: pdm.Out <:< Directive1[T]): DValidated[T] = {
    val d1 = akka.http.scaladsl.server.Directives.parameter(pdm)
    DValidated.fromDirective(d1)
  }

  def formField[T](pdm: FieldMagnet)(implicit cv: pdm.Out <:< Directive1[T]): DValidated[T] = {
    val d1 = akka.http.scaladsl.server.Directives.formField(pdm)
    DValidated.fromDirective(d1)
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