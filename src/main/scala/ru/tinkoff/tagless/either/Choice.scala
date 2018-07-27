package ru.tinkoff.tagless.either

trait Choice[A, B, C] {
  def left(a: A): C
  def right(b: B): C
}

object Choice {
  def apply[A, B, C](implicit choice: Choice[A, B, C]): Choice[A, B, C] = choice

  def left[A, B, C: Choice[A, B, ?]](a: A): C  = Choice[A, B, C].left(a)
  def right[A, B, C: Choice[A, B, ?]](b: B): C = Choice[A, B, C].right(b)

  implicit def continuation[A, B, C, D](implicit choice: Choice[A, B, C]): Choice[A, D, D <|- C] =
    new Choice[A, D, D <|- C] {
      override def left(a: A)  = _ => choice.left(a)
      override def right(d: D) = cont => cont(d)
    }

  type <|-[A, B] = (A => B) => B

  implicit class ContSyntax[A, B](val cont: A <|- B) extends AnyVal {
    def flatMap(f: A => B): B = cont(f)
    def map(f: A => B): B     = cont(f)
  }
}

object AlternatingApp {
  import Choice.{<|-, ContSyntax, left, right}

  def sqrt[C: Choice[String, Double, ?]](x: Double): C =
    if (x < 0) left(s"$x < 0 under sqrt") else right(Math.sqrt(x))

  def div[C: Choice[String, Double, ?]](x: Double, y: Double): C =
    if (y == 0) left(s"division by zero") else right(x / y)

  def calc[C: Choice[String, Double, ?]](x: Double, y: Double): C =
    for {
      xq <- sqrt[Double <|- C](x)
      yq <- sqrt[Double <|- C](y)
      sq <- sqrt[Double <|- C](x + y)
    } yield div[C](xq + yq, sq)

  def main(args: Array[String]): Unit = {
    implicit val choiceString: Choice[String, Double, String] =
      new Choice[String, Double, String] {
        override def left(a: String): String  = s"left{$a}"
        override def right(b: Double): String = s"right{$b}"
      }

    println(calc(1, 2))
    println(calc(1, -2))
    println(calc(0, 0))
  }
}
