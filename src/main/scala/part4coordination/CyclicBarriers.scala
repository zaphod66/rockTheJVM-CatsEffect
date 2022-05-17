package part4coordination

import cats.effect.std.CyclicBarrier
import cats.effect.{Deferred, IO, IOApp, Ref}
import cats.syntax.parallel.*
import utils.debug

import scala.util.Random
import scala.concurrent.duration.*

object CyclicBarriers extends IOApp.Simple {

  def worker(id: Int, barrier: CyclicBarrier[IO]): IO[Unit] = for {
    _ <- IO(s"[worker $id] preparing...").debug
    _ <- IO.sleep((Random.nextDouble() * 100).toInt.millis)
    _ <- IO(s"[worker $id] waiting...").debug
    _ <- barrier.await
    _ <- IO(s"[worker $id] --> working...").debug
  } yield ()

  def batch(barrier: CyclicBarrier[IO]): IO[Unit] = for {
    _       <- IO("Start batching workers").debug
    _       <- (1 to 10).toList.parTraverse(worker(_, barrier))
    _       <- IO("Finished batching workers").debug
  } yield ()

  def batch1(): IO[Unit] = for {
    barrier <- CyclicBarrier[IO](5)
    _       <- batch(barrier)
  } yield ()

  def batch2(): IO[Unit] = for {
    barrier <- MyCyclicBarrier(5)
    _       <- batch(barrier)
  } yield ()

  override def run: IO[Unit] =
    IO("CyclicBarriers").debug *>
    IO("1-------------").debug *>
    batch1() *>
    IO("2-------------").debug *>
    batch2() *>
    IO("3-------------").debug *>
    IO.unit
}

abstract class MyCyclicBarrier extends CyclicBarrier[IO]

object MyCyclicBarrier {

  case class State(remaining: Int, signal: Deferred[IO, Unit])

  private def createSignal(): IO[Deferred[IO, Unit]] = Deferred[IO, Unit]

  def apply(n: Int): IO[MyCyclicBarrier] = for {
    signal <- createSignal()
    state  <- Ref[IO].of(State(n, signal))
  } yield new MyCyclicBarrier {
    override def await: IO[Unit] = IO("await ->").debug *> createSignal().flatMap { newSignal =>
      state.modify {
        case State(1, signal) => State(n, newSignal) -> signal.complete(()).void
        case State(n, signal) => State(n - 1, signal) -> signal.get // cancellation is NOT taken into account
      }.flatten *> IO("<- await").debug.void
    }
  }
}
