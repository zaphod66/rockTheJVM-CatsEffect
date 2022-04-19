package part4coordination

import cats.Monoid
import cats.effect.{IO, IOApp}
import cats.effect.std.Semaphore

import scala.concurrent.duration.*
import scala.util.Random
import utils.debug

object Semaphores extends IOApp.Simple {

  val semaphore: IO[Semaphore[IO]] = Semaphore[IO](3)

  def doWork(): IO[Int] = IO.sleep(200.millis + Random.nextInt(200).millis) *> IO(Random.nextInt(100))

  def schedule(id: Int, sem: Semaphore[IO]): IO[Int] = for {
    a0  <- sem.available
    c0  <- sem.count
    _   <- IO(s"[session $id] acquiring access ($c0 callers, $a0 available)").debug
    _   <- sem.acquire
    a1  <- sem.available
    c1  <- sem.count
    _   <- IO(s"[session $id] access acquired ($c1 callers, $a1 available)").debug
    res <- doWork()
    a2  <- sem.available
    c2  <- sem.count
    _   <- IO(s"[session $id] releasing access ($c2 callers, $a2 available)").debug
    _   <- sem.release
    a3  <- sem.available
    c3  <- sem.count
    _   <- IO(s"[session $id] access released ($c3 callers, $a3 available)").debug
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
