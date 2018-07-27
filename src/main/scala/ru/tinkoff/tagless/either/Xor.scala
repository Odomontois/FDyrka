package ru.tinkoff.tagless.either

trait Xor[A, B] { self =>
  def fold[X](l: A => X)(r: B => X): X

  def toEither = fold[Either[A, B]](Left(_))(Right(_))

  def map[C](f: B => C): Xor[A, C] = new Xor[A, C] {
    override def fold[X](l: A => X)(r: C => X): X = self.fold(l)(b => r(f(b)))
  }

  def flatMap[C](f: B => Xor[A, C]): Xor[A, C] = new Xor[A, C] {
    override def fold[X](l: A => X)(r: C => X): X = self.fold(l)(b => f(b).fold(l)(r))
  }
  override def toString: String = fold(a => s"left($a)")(b => s"right($b)")
}

object Xor {
  def fromEither[A, B](e: Either[A, B]): Xor[A, B] = new Xor[A, B] {
    override def fold[C](f: A => C)(g: B => C): C = e.fold(f, g)
  }

  def left[A, B](a: A): Xor[A, B] = new Xor[A, B] {
    override def fold[C](f: A => C)(g: B => C): C = f(a)
  }

  def right[A, B](b: B): Xor[A, B] = new Xor[A, B] {
    override def fold[C](f: A => C)(g: B => C): C = g(b)
  }
}

object XorApp {

  def sqrt(x: Double): Xor[String, Double] =
    if (x < 0) Xor.left(s"$x < 0 under sqrt") else Xor.right(Math.sqrt(x))

  def div(x: Double, y: Double): Xor[String, Double] =
    if (y == 0) Xor.left(s"division by zero") else Xor.right(x / y)

  def calc(x: Double, y: Double): Xor[String, Double] =
    for {
      xq  <- sqrt(x)
      yq  <- sqrt(y)
      sq  <- sqrt(x + y)
      res <- div(xq + yq, sq)
    } yield res

  def main(args: Array[String]): Unit = {
    println(calc(1, 2))
    println(calc(1, -2))
    println(calc(0, 0))
  }
}
