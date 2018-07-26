package org.manatki.tagged
import Operation.{Change, Create, Transact}
import cats.data.{EitherT, State, WriterT}
import cats.kernel.Monoid
import cats.implicits._

object Interpreter {
  type Accounts = Map[String, BigDecimal]

  type Result[A] = EitherT[WriterT[State[Accounts, ?], List[String], ?], String, A]

  implicit def stateMonoid[A: Monoid]: Monoid[State[Accounts, A]] = new Monoid[State[Accounts, A]] {
    override def empty: State[Accounts, A] = State.pure(Monoid.empty[A])
    override def combine(x: State[Accounts, A], y: State[Accounts, A]): State[Accounts, A] =
      for (xx <- x; yy <- y) yield xx |+| yy
  }

  def get: Result[Accounts]            = EitherT.right(WriterT.liftF(State.get))
  def set(acc: Accounts): Result[Unit] = EitherT.right(WriterT.liftF(State.set(acc)))
  def raise(s: String): Result[Unit]   = s.raiseError[Result, Unit]
  def log(s: String): Result[Unit]     = EitherT.right(WriterT.tell(List(s)))

  def accumulate(result: Result[Unit]): Result[Unit] =
    for {
      start <- get
      _     <- result.handleErrorWith(s => log(s) *> set(start))
    } yield ()

  def failFastInterpreter: Operation => Result[Unit] = {
    case Create(id)       => operate.create(id)
    case Change(id, diff) => operate.change(id, diff)
    case Transact(ops)    => operate.transact(ops.foldMap(failFastInterpreter))
  }

  def accumulatingInterpreter: Operation => Result[Unit] = failFastInterpreter andThen accumulate

  val operate: Operate[Result[Unit]] = new Operate[Result[Unit]] {
    override def create(id: String): Result[Unit] =
      for {
        acc <- get
        _   <- raise(s"Account $id already exists").whenA(acc contains id)
        _   <- set(acc + (id -> BigDecimal(0)))
      } yield ()

    override def change(id: String, diff: BigDecimal): Result[Unit] =
      for {
        acc  <- get
        cur  <- acc.get(id).liftTo[Result](s"Account $id doesn't exists")
        next = cur + diff
        _    <- raise(s"Insufficient funds at $id: $cur to change by $diff").whenA(next < 0)
        _    <- set(acc + (id -> next))
      } yield ()

    override def transact(xs: Result[Unit]): Result[Unit] = accumulate(xs)
  }

  implicit val raiseString: Raise[Result[Unit], String] = raise
  implicit val accumulatingOperate: Operate[Result[Unit]] = operate.imap(accumulate)(identity)
}

trait Raise[A, E]{
  def raise(err: E): A
}

object Raise{
  def raise[A, E](e: E)(implicit r: Raise[A, E]): A = r.raise(e)
}
