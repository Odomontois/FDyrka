package ru.tinkoff.tagless.functor
import cats.data.{Kleisli, OptionT}
import cats.effect.Sync
import org.http4s.{HttpRoutes, QueryParamDecoder}
import org.http4s.dsl.Http4sDsl
import cats.syntax.flatMap._
import cats.syntax.applicativeError._
import org.http4s.dsl.impl.QueryParamDecoderMatcher

final case class AccountsModule[F[_]: Sync](accounts: Accounts[F])(implicit dsl: Http4sDsl[F]) {
  import dsl._




  val service: HttpRoutes[F] = HttpRoutes
    .of[F] {
      case GET -> Root / "account" / id => accounts.get(id).flatMap(bd => Ok(bd.toString))
      case POST -> Root / "account" / id :? IntPara => accounts.get(id).flatMap(bd => Ok(bd.toString))
      case _                            => Ok("lol")
    }
    .mapF(_.recoverWith { case AccountError.DoesNotExist(id) => OptionT.liftF(NotFound(s"account $id not found")) })
}

object AccountsModule{
  object DiffMatcher extends QueryParamDecoderMatcher[BigDecimal]("diff")

  implicit val bigDecimalQueryParameter: QueryParamDecoder[BigDecimal] = QueryParamDecoder.fromUnsafeCast()
}
