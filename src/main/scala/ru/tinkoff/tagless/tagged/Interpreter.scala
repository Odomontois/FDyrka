package ru.tinkoff.tagless.tagged
import cats.data.{State, StateT}
import cats.implicits._
import cats.kernel.Monoid
import ru.tinkoff.tagless.tagged.Interpreter.implicits._
import ru.tinkoff.tagless.tagged.Operation.{Change, Create, Transact}

object Interpreter {
  import State.{inspect, modify}
  type Accounts = Map[String, BigDecimal]


  final case class Transaction(accounts: Accounts = Map.empty, errors: Vector[String] = Vector()) {
    def raise(s: String) = copy(errors = errors :+ s)
  }
  type Result[A] = State[Transaction, A]

  def setAccounts(acc: Accounts): Result[Unit] = modify(_.copy(accounts = acc))
  def getAccounts: Result[Accounts]            = inspect(_.accounts)

  def raise(s: String): Result[Unit] = State.modify(_.raise(s))

  def interpreter: Operation => Result[Unit] = {
    case Create(id)       => operate.create(id)
    case Change(id, diff) => operate.change(id, diff)
    case Transact(ops)    => operate.transact(ops.traverse_(interpreter))
  }

  object implicits {
    implicit val raiseString: Raise[Result[Unit], String] = raise

    implicit def resultMonoid[A: Monoid]: Monoid[Result[A]] = new Monoid[Result[A]] {
      override def empty: Result[A] = Monoid.empty[A].pure[Result]
      override def combine(x: Result[A], y: Result[A]): Result[A] =
        for (xx <- x; yy <- y) yield xx |+| yy
    }

    implicit val operate: Operate[Result[Unit]] = new Operate[Result[Unit]] {
      override def create(id: String): Result[Unit] =
        for {
          acc <- getAccounts
          _ <- if (acc contains id) raise(s"Account $id already exists")
              else setAccounts(acc + (id -> BigDecimal(0)))
        } yield ()

      override def change(id: String, diff: BigDecimal): Result[Unit] =
        for {
          acc <- getAccounts
          _ <- acc.get(id).fold(raise(s"Account $id doesn't exists")) { cur =>
                val next = cur + diff
                if (next < 0) raise(s"Insufficient funds at $id: $cur to change by $diff")
                else setAccounts(acc + (id -> next))
              }
        } yield ()

      override def transact(xs: Result[Unit]): Result[Unit] = StateT.modifyF {
        case Transaction(acc, errs) =>
          xs.runS(Transaction(acc, Vector())).map {
            case Transaction(newAcc, Vector()) => Transaction(newAcc, errs)
            case Transaction(_, errs2)         => Transaction(acc, errs ++ errs2)
          }
      }
    }
  }
}

trait Raise[A, E] {
  def raise(err: E): A
}

object Raise {
  def raise[A, E](e: E)(implicit r: Raise[A, E]): A = r.raise(e)
}
