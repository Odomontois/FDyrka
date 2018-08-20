package ru.tinkoff.tagless

package object tagged {
  type Algebra[F[_], A] = F[A] => A
  type Coalgebra[F[_], A] = A => F[A]
}
