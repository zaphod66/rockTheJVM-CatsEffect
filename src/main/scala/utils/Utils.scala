package utils

import cats.effect.IO
extension [A](ioa: IO[A])
  def debug: IO[A] = for {
    a <- ioa
    t = Thread.currentThread().getName
    _ = println(s"[$t] - $a")
  } yield a
