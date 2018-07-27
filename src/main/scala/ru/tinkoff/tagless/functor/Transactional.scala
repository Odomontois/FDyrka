package ru.tinkoff.tagless.functor
import mainecoon.{autoFunctorK, autoInvariantK}

trait Transactional[I, F[_]] {
  def begin: F[(I, F[Boolean])]
}


