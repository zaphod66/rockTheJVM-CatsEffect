package utils

import cats.effect.IO

import scala.concurrent.duration.FiniteDuration

extension [A](ioa: IO[A])
  def debug: IO[A] = for {
    a <- ioa
    t = Thread.currentThread().getName
    _ = println(s"[$t] - $a")
  } yield a

def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] =
  (
    IO(s"starting computation for: $value").debug >>
      IO.sleep(duration) >>
      IO(s"computation finished for: $value").debug >>
      IO(value)
    ).onCancel(IO(s"computation canceled for: $value").debug.void)

def timedRun[A](io: IO[A], duration: FiniteDuration): IO[Either[A, Unit]] =
  IO.race(io, IO.sleep(duration))
