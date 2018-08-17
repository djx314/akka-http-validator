package net.scalax.akka.http.validator.helper

import akka.http.scaladsl.server._
import cats.data.Validated
import net.scalax.akka.http.validator.core.{ DecoderShape, ErrorMessage }
import shapeless._

trait HListDecoderImplicit {

  private def parallelValidate[A, B, C](v1: Validated[ErrorMessage, A], v2: Validated[ErrorMessage, B])(f: (A, B) => C): Validated[ErrorMessage, C] =
    (v1, v2) match {
      case (Validated.Valid(a), Validated.Valid(b)) => Validated.Valid(f(a, b))
      case (Validated.Valid(_), i @ Validated.Invalid(_)) => i
      case (i @ Validated.Invalid(_), Validated.Valid(_)) => i
      case (Validated.Invalid(e1), Validated.Invalid(e2)) => Validated.Invalid(ErrorMessage(e1.msg ::: e2.msg))
    }

  implicit def hnilDirective1Implicit: DecoderShape.Aux[HNil, HNil, HNil] = new DecoderShape[HNil, HNil] {
    override type Target = HNil
    override def wrapRep(baseRep: HNil): HNil = baseRep
    override def toDirective(targetRep: HNil): Directive1[Validated[ErrorMessage, HNil]] =
      Directive.apply { (s: Tuple1[Validated[ErrorMessage, HNil]] => Route) => s(Tuple1(Validated.Valid(HNil))) }
  }

  implicit def hlistDirective1Implicit[A, B, C, D <: HList, E <: HList, F <: HList](implicit head: Lazy[DecoderShape.Aux[A, B, C]], tail: Lazy[DecoderShape.Aux[D, E, F]]): DecoderShape.Aux[A :: D, B :: E, C :: F] = new DecoderShape[A :: D, B :: E] {
    override type Target = C :: F
    override def wrapRep(baseRep: A :: D): C :: F = {
      val a :: d = baseRep
      head.value.wrapRep(a) :: tail.value.wrapRep(d)
    }
    override def toDirective(targetRep: C :: F): Directive1[Validated[ErrorMessage, B :: E]] = {
      val c :: f = targetRep
      val cd = head.value.toDirective(c)
      val fd = tail.value.toDirective(f)
      cd.flatMap { cr => fd.map(fr => parallelValidate(cr, fr)((cv, fv) => cv :: fv)) }
    }
  }

}