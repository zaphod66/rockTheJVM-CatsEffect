package part2effects

import cats.effect.IO

import scala.util.{Failure, Success, Try}

object IOErrors {

  val aFailedComp1: IO[Int] = IO.delay(throw new RuntimeException("Boom"))
  val aFailedComp2: IO[Int] = IO.raiseError(new RuntimeException("Boom2"))

  val handledIO1: IO[AnyVal] = aFailedComp2.handleErrorWith {
    case _: RuntimeException => IO.delay(println("handled exception"))
  }

  val effectAsEither: IO[Either[Throwable, Int]] = aFailedComp2.attempt
  val compAsString: IO[String] = aFailedComp2.redeem(ex => s"Fail: ${ex.getMessage}", v => s"SUCCESS: $v")

  // ==============

  def option2IO[A](option: Option[A])(ifEmpty: Throwable): IO[A] = option.fold(IO.raiseError(ifEmpty))(IO.pure)
  def try2IO[A](t: Try[A]): IO[A] = t.fold(IO.raiseError, IO.pure)
  def either2IO[A](e: Either[Throwable, A]): IO[A] = e.fold(IO.raiseError, IO.pure)

  def handleIOError[A](ioa: IO[A])(handler: Throwable => A): IO[A] = ioa.redeem(handler, identity)
  def handleIOErrorWith[A](ioa: IO[A])(handler: Throwable => IO[A]): IO[A] = ioa.redeemWith(handler, _ => ioa)

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global

    handledIO1.unsafeRunSync()

    val o1IO = option2IO(Option(42))(new RuntimeException("Empty Option"))
    val o2IO = option2IO(Option.empty[Int])(new RuntimeException("Empty Option"))
    val t1IO = try2IO(Success(42))
    val t2IO = try2IO(Failure[Int](new RuntimeException("Failed Try")))
    val e1IO = either2IO(Right[Throwable, Int](42))
    val e2IO = either2IO(Left[Throwable, Int](new RuntimeException("Lefty")))

    val h1IO = handleIOError(IO(21))(_ => 42)
    val h2IO = handleIOError(aFailedComp2)(_ => 42)
    val h3IO = handleIOErrorWith(IO(21))(_ => IO(42))
    val h4IO = handleIOErrorWith(aFailedComp2)(_ => IO(42))
  }
}
