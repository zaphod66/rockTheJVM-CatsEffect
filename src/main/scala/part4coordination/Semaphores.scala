package part4coordination

import cats.Monoid
import cats.effect.{IO, IOApp}
import cats.effect.std.Semaphore

import scala.concurrent.duration.*
import scala.util.Random
import utils.debug

object Semaphores extends IOApp.Simple {

  val semaphore: IO[Semaphore[IO]] = Semaphore[IO](2)   // two permits

  def doWork(): IO[Int] = IO.sleep(200.millis + Random.nextInt(200).millis) *> IO(Random.nextInt(100))

  def schedule(id: Int, sem: Semaphore[IO]): IO[Int] = for {
    _   <- IO(s"[session $id] waiting for access").debug
    _   <- sem.acquire
    a   <- sem.available
    c   <- sem.count
    _   <- IO(s"[session $id] access acquired ($c callers, $a available)").debug
    res <- doWork()
    _   <- IO(s"[session $id] releasing access").debug
    _   <- sem.release
    _   <- IO(s"[session $id] produced $res").debug
  } yield res

  def reduceAll(tasks: List[IO[Unit]]): IO[Unit] = {
    tasks.reduce(IO.both(_, _).void)
  }

  def tasks1(sem: Semaphore[IO]): List[IO[Unit]] = (1 to 10).map(i => schedule(i, sem).void).toList
  def tasks2(sem: Semaphore[IO]): List[IO[Int]] = (1 to 10).map(i => schedule(i, sem)).toList

  val scheduledTasks1: IO[Unit] = for {
    sem <- semaphore
    ts = tasks1(sem)
    _   <- reduceAll(ts)
  } yield ()

  import cats.syntax.parallel._
  val scheduledTasks2: IO[List[Int]] = for {
    sem <- semaphore
    res <- tasks2(sem).parSequence
  } yield res

  override def run: IO[Unit] =
    IO("Semaphores").debug *>
    IO("1----------").debug *>
    scheduledTasks1 *>
    IO("2----------").debug *>
    scheduledTasks2.debug *>
    IO("3----------").debug *>
    IO.unit
}
