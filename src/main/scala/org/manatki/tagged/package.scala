package org.manatki

package object tagged {
  type Algebra[F[_], A] = F[A] => A
}
