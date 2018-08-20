package ru.tinkoff.tagless.tagged
import cats.Id
import cats.arrow.FunctionK

trait CoalgT[PreAlg[_, _], A] extends (A => FunctionK[PreAlg[A, ?], Id]) {
  def run[B](a: A, alg: PreAlg[A, B]): B
  override def apply(v1: A): FunctionK[PreAlg[A, ?], Id] = new FunctionK[PreAlg[A, ?], Id] {
    override def apply[B](fa: PreAlg[A, B]): Id[B] = run(v1, fa)
  }
}
