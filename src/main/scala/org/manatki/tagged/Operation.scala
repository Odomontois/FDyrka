package org.manatki
package tagged
import cats.Functor
import cats.syntax.functor._
import mainecoon.autoInvariant
import org.manatki.tagged
import simulacrum.typeclass

sealed trait Operation

object Operation {
  final case class Create(id: String)                   extends Operation
  final case class Change(id: String, diff: BigDecimal) extends Operation
  final case class Transact(ops: List[Operation])       extends Operation
}

sealed trait OperationF[A]

object OperationF {
  final case class Create[A](id: String)                   extends OperationF[A]
  final case class Change[A](id: String, diff: BigDecimal) extends OperationF[A]
  final case class Transact[A](ops: List[A])               extends OperationF[A]

  implicit val functor: Functor[OperationF] = new Functor[OperationF] {
    override def map[A, B](fa: OperationF[A])(f: A => B): OperationF[B] =
      fa match {
        case Create(id)       => Create(id)
        case Change(id, diff) => Change(id, diff)
        case Transact(lst)    => Transact(lst.map(f))
      }
  }

  def fromRec(op: Operation): Fix[OperationF] =
    Fix(op match {
      case Operation.Create(id)       => Create(id)
      case Operation.Change(id, diff) => Change(id, diff)
      case Operation.Transact(ops)    => Transact(ops.map(fromRec))
    })

  def toRec(fix: Fix[OperationF]): Operation =
    fix.value match {
      case Create(id)       => Operation.Create(id)
      case Change(id, diff) => Operation.Change(id, diff)
      case Transact(ops)    => Operation.Transact(ops.map(toRec))
    }
}

final case class Fix[F[_]](value: F[Fix[F]]) {
  def fold[A](algebra: Algebra[F, A])(implicit F: Functor[F]): A =
    algebra(value.map(_.fold(algebra)))
}

object Fix {
  def mu[F[_]: Functor, A](algebra: Algebra[F, A]): AlgebraMorphism[F, Fix[F], A] = AlgebraMorphism(
    from = (ff: F[Fix[F]]) => Fix(ff),
    to = algebra,
    f = _.fold(algebra)
  )
}

final case class AlgebraMorphism[F[_], A, B](from: Algebra[F, A], to: Algebra[F, B], f: A => B) {
  def guaranteed(fa: F[A])(implicit F: Functor[F]): Boolean =
    f(from(fa)) == to(fa.map(f))
}

trait Operational[A] {
  def create(id: String): A
  def change(id: String, diff: BigDecimal): A
  def transact(xs: List[A]): A
}

object Operational {
  def create[A](id: String)(implicit op: Operational[A]): A                   = op.create(id)
  def change[A](id: String, diff: BigDecimal)(implicit op: Operational[A]): A = op.change(id, diff)
  def transact[A](xs: List[A])(implicit op: Operational[A]): A                = op.transact(xs)

  import OperationF._

  def fromAlgebra[A](alg: Algebra[OperationF, A]): Operational[A] =
    new Operational[A] {
      override def create(id: String): A                   = alg(Create(id))
      override def change(id: String, diff: BigDecimal): A = alg(Change(id, diff))
      override def transact(xs: List[A]): A                = alg(Transact(xs))
    }

  def toAgebra[A](op: Operational[A]): Algebra[OperationF, A] = {
    case Create(id)       => op.create(id)
    case Change(id, diff) => op.change(id, diff)
    case Transact(ops)    => op.transact(ops)
  }
}

@autoInvariant
trait Operate[A] {
  def create(id: String): A
  def change(id: String, diff: BigDecimal): A
  def transact(xs: A): A
}

object Operate {
  def create[A](id: String)(implicit op: Operate[A]): A                   = op.create(id)
  def change[A](id: String, diff: BigDecimal)(implicit op: Operate[A]): A = op.change(id, diff)
  def transact[A](xs: A)(implicit op: Operate[A]): A                      = op.transact(xs)
}


