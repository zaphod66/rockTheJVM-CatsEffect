package part4coordination

import cats.effect.{IO, IOApp}
import cats.effect.std.Semaphore

import scala.concurrent.duration._
import scala.util.Random

import utils.debug

object Semaphores extends IOApp.Simple {

  val semaphore: IO[Semaphore[IO]] = Semaphore[IO](2)   // two permits

  def doWork(): IO[Int] = IO.sleep(1.second) *> IO(Random.nextInt(100))

  def schedule(id: Int, sem: Semaphore[IO]): IO[Int] = for {
    _   <- IO(s"[session $id] waiting for access").debug
    _   <- sem.acquire
    _   <- IO(s"[session $id] access acquired").debug
    res <- doWork()
    _   <- IO(s"[session $id] access released").debug
    _   <- sem.release
    _   <- IO(s"[session $id] produced $res").debug
  } yield res

  def runSchedule(): IO[Unit] = for {
    sem  <- semaphore
    fib1 <- schedule(1, sem).start
    fib2 <- schedule(2, sem).start
    fib3 <- schedule(3, sem).start
    fib4 <- schedule(4, sem).start
    _    <- fib1.join
    _    <- fib2.join
    _    <- fib3.join
    _    <- fib4.join
  } yield ()

  override def run: IO[Unit] =
    IO("Semaphores").debug *>
    IO("---------").debug *>
    runSchedule() *>
    IO("---------").debug *>
    IO.unit
}
