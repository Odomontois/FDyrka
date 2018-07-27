package ru.tinkoff.tagless.either

object EitherApp {
  def sqrt(x: Double): Either[String, Double] =
    if (x < 0) Left(s"$x < 0 under sqrt") else Right(Math.sqrt(x))

  def div(x: Double, y: Double): Either[String, Double] =
    if (y == 0) Left(s"division by zero") else Right(x / y)

  def calc(x: Double, y: Double): Either[String, Double] =
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
