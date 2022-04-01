package part4coordination

import cats.effect.{IO, IOApp, Deferred}

import utils.debug
import scala.concurrent.duration._

object Defers extends IOApp.Simple {

  def demoDeferred(): IO[Unit] = {
    def consumer(signal: Deferred[IO, Int]) = for {
      _ <- IO("[consumer] waiting for result...").debug
      v <- signal.get
      _ <- IO(s"[consumer] got the result: $v").debug
    } yield ()

    val v = 42
    def producer(signal: Deferred[IO, Int]) = for {
      _ <- IO("[producer] Crunching numbers ...").debug
      _ <- IO.sleep(500.millis)
      _ <- signal.complete(v)
      _ <- IO(s"[producer] complete with ($v).").debug
    } yield ()

    for {
      s <- Deferred[IO, Int]
      _ <- IO.race(consumer(s), producer(s))
    } yield ()
  }

  override def run: IO[Unit] =
    IO("Defers").debug *>
    IO("1-----------").debug *>
    demoDeferred() *>
    IO("2-----------").debug *>
    IO.unit
}
