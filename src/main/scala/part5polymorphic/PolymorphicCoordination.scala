package part5polymorphic

import cats.effect.{Concurrent, Deferred, IO, IOApp, Ref, Spawn}

import scala.concurrent.duration._

object PolymorphicCoordination extends IOApp.Simple {

  trait MyConcurrent[F[_]] extends Spawn[F] {
    def ref[A](a: A): F[Ref[F, A]]
    def deferred[A]: F[Deferred[F, A]]
  }

  val concurrentIO: Concurrent[IO] = Concurrent[IO]
  val aDeferred1: IO[Deferred[IO, Int]] = Deferred[IO, Int]  // given Concurrent[IO] in scope
  val aDeferred2: IO[Deferred[IO, Int]] = concurrentIO.deferred[Int]

  // -----------------------------------------

  import cats.syntax.functor._
  import cats.syntax.flatMap._

  import utils.general._

  def alarmCounter[F[_]](limit: Int)(using concurrent: Concurrent[F]): F[Unit] = {
    def tick(countRef: Ref[F, Int], signal: Deferred[F, Unit]): F[Unit] = for {
      _ <- unsafeSleep(200.millis)
      c <- countRef.updateAndGet(_ + 1)
      _ <- concurrent.pure(s"[tick] incremented counter to $c").debug
      _ <- if (c >= limit) concurrent.pure(s"[tick] counter reached $limit").debug.flatMap(_ => signal.complete(()).void) else tick(countRef, signal)
    } yield ()

    def wait(signal: Deferred[F, Unit]): F[Unit] = for {
      _ <- concurrent.pure("[wait] waiting for completion ...").debug
      _ <- signal.get
      _ <- concurrent.pure("[wait] time's up.").debug
    } yield ()

    for {
      countRef <- concurrent.ref(0)
      signal   <- concurrent.deferred[Unit]
      _        <- concurrent.both(tick(countRef, signal), wait(signal))
    } yield ()
  }

  override def run: IO[Unit] =
    IO("Polymorphic Coordination").debug *>
    IO("1-----------------------").debug *>
    alarmCounter[IO](3) *>
    IO("2-----------------------").debug *>
    IO.unit
}
