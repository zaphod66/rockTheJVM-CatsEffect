package part2effects

import cats.effect.{IO, IOApp}

object IOIntro extends IOApp.Simple {

  val IO1st: IO[Int] = IO.pure(42) // arg must not have side effects
  val IO2nd: IO[Int] = IO.delay({
    println("This is a side effect")
    42
  })
  val IODontDo: IO[Int] = IO.pure({
    println("This is a side effect")
    42
  })

  def sequenceAndLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] = for {
    _ <- ioa
    b <- iob
  } yield b

  def sequenceAndLast2[A, B](ioa: IO[A], iob: IO[B]): IO[B] = ioa.flatMap(_ => iob)

  def sequenceAndFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] = for {
    a <- ioa
    _ <- iob
  } yield a

  def sequenceAndFirst2[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa.flatMap(a => iob.as(a))

  def forever1[A](ioa: IO[A]): IO[A] = ioa.flatMap(_ => forever1(ioa))
  def forever2[A](ioa: IO[A]): IO[A] = ioa >> forever2(ioa)
  def forever3[A](ioa: IO[A]): IO[A] = ioa.foreverM

  def convert[A, B](ioa: IO[A], b: B): IO[B] = ioa.map(_ => b)

  def asUnit[A](ioa: IO[A]): IO[Unit] = convert(ioa, ())
  def asUnit2[A](ioa: IO[A]): IO[Unit] = ioa.void

  def sum(n: Int): Int =
    if (n <= 0) 0
    else n + sum(n - 1)

  def sumIO(n: Int): IO[Int] =
    if (n <= 0) IO(0)
    else for {
      nn <- IO(n)
      prev <- sumIO(n - 1)
    } yield nn + prev

  def fib(n: Int): IO[BigInt] =
    if (n <= 2) IO(1)
    else for {
      n1 <- IO.defer(fib(n - 1))  //  same as    n1 <- IO(fib(n - 1)).flatten
      n2 <- IO.defer(fib(n - 2))  //  same as    n2 <- IO(fib(n - 2)).flatten
    } yield n1 + n2

  override def run: IO[Unit] = {
    import cats.effect.unsafe.implicits.global  // "platform"

    IO.println(s"aDelayedIO ${IO2nd.unsafeRunSync()}") *> IO.println(fib(20).unsafeRunSync())
  }
}
