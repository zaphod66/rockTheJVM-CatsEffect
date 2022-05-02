package part5polymorphic

import cats.effect.{Concurrent, IO, IOApp, Temporal}

import scala.concurrent.duration.*

import utils.{debug, runWithSleep}

object PolymorphicTemporals extends IOApp.Simple {

  trait MyTemporal[F[_]] extends Concurrent[F] {
    def sleep(time: FiniteDuration): F[Unit]
  }

  val temporalIO: Temporal[IO] = Temporal[IO]

  val chain1: IO[String] = IO("Loading...").debug *> IO.sleep(1.second) *> IO("Ready!").debug
  val chain2: IO[String] = temporalIO.pure("Loading...").debug *> temporalIO.sleep(1.second) *> temporalIO.pure("Ready!").debug

  // -------------------------

  import cats.syntax.flatMap._

  def timeout[F[_], A](fa: F[A], duration: FiniteDuration)(using temporal: Temporal[F]): F[A] =
    temporal.race(fa, temporal.sleep(duration)).flatMap {
      case Left(value) => temporal.pure(value)
      case Right(_)    => temporal.raiseError(new RuntimeException("Computation timed out"))
    }

  val v1: IO[Int] = runWithSleep(42, 100.millis)

  override def run: IO[Unit] =
    IO("Polymorphic Temporal").debug *>
    IO("1-----------------------").debug *>
    timeout(v1, 50.millis).handleError(_.getMessage).debug *>
    IO("3 -------").debug *>
    timeout(v1, 200.millis) *>
    IO("2-----------------------").debug *>
    IO.unit
}
