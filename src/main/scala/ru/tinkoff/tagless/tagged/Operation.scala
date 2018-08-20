package ru.tinkoff.tagless
package tagged
import cats.Functor
import cats.syntax.functor._
import mainecoon.autoInvariant
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

final case class CoalgebraMorphism[F[_], A, B](from: Coalgebra[F, A], to: Coalgebra[F, B], f: A => B) {
  def guaranteed(a: A)(implicit F: Functor[F]): Boolean =
    from(a).map(f) == to(f(a))
}

trait PreOperational[A, B] {
  def create(id: String): B
  def change(id: String, diff: BigDecimal): B
  def transact(xs: List[A]): B
}

object Operational {
  type Operational[A] = PreOperational[A, A]
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

  def fromCoalgebra[A](coalg: Coalgebra[OperationF, A]): CoalgT[PreOperational, A] =
    new CoalgT[PreOperational, A] {
      override def run[B](a: A, op: PreOperational[A, B]): B = coalg(a) match {
        case Create(id)       => op.create(id)
        case Change(id, diff) => op.change(id, diff)
        case Transact(ops)    => op.transact(ops)
      }
    }

  def toCoalgebra[A](coalg: CoalgT[PreOperational, A]): Coalgebra[OperationF, A] = { a =>
    val prealg = new PreOperational[A, OperationF[A]] {
      override def create(id: String): OperationF[A]                   = OperationF.Create(id)
      override def change(id: String, diff: BigDecimal): OperationF[A] = OperationF.Change(id, diff)
      override def transact(xs: List[A]): OperationF[A]                = OperationF.Transact(xs)
    }
    coalg.run(a, prealg)
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
