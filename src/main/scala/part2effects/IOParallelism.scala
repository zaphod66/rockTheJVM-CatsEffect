package part2effects

import cats.effect.IO.Par
import cats.{Apply, Parallel}
import cats.effect.{IO, IOApp}
import utils.debug

object IOParallelism extends IOApp.Simple {

  override def run: IO[Unit] = composedIO1.map(println) *>
    composedIO2.debug.map(println) *>
    composedIO3.debug.map(println) *>
    composedIO4.debug.map(println) *>
    composedIO5.debug.map(println)

  val io1: IO[String] = IO(s"IO1").debug
  val io2: IO[String] = IO(s"IO2").debug

  import cats.syntax.apply._
  def combine[F[_]: Apply, A, B](ioa: F[A], iob: F[B]): F[String] = (ioa, iob).mapN((a, b) => s"$a - $b")

  val composedIO1: IO[String] = combine(io1, io2)

  val io1Par: IO.Par[String] = Parallel[IO].parallel(io1)
  val io2Par: IO.Par[String] = Parallel[IO].parallel(io2)
  import cats.effect.implicits._
  val composedIO2Par: IO.Par[String] = combine(io1Par, io2Par)
  val composedIO2: IO[String] = Parallel[IO].sequential(composedIO2Par)

  val io3: IO[String] = IO.raiseError(new RuntimeException("Boom 1"))
  val io4: IO[String] = IO.raiseError(new RuntimeException("Boom 2"))

  import cats.syntax.parallel._
  val composedIO3: IO[String] = (io1, io2).parMapN((a, b) => s"$a - $b")

  val composedIO4: IO[String] = (io1, io3).parMapN(_ + _).handleError(_.getMessage) //.redeem(_.getMessage, identity)
  val composedIO5: IO[String] = (IO(Thread.sleep(100)) *> io3, io4).parMapN(_ + _).handleError(_.getMessage) //.redeem(_.getMessage, identity)
}
