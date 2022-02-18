package part3concurrecny

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{IO, IOApp, Outcome}

import scala.concurrent.duration.*
import utils.debug

object Fibers extends IOApp.Simple {

  def testCancel(): IO[Outcome[IO, Throwable, String]] = {
    val task = IO("starting").debug >> IO.sleep(1.second) >> IO("done")
    val taskWithHandler = task.onCancel(IO("I'm being cancelled!").debug.void)

    for {
      fiber <- taskWithHandler.start
      _ <- IO.sleep(500.millis) >> IO("cancelling").debug
      _ <- fiber.cancel
      result <- fiber.join
    } yield result
  }

  def processResultsFromFiber[A](io: IO[A]): IO[A] = {
    val resultIO = for {
      fiber <- io.start.debug
      result <- fiber.join
    } yield result

    resultIO.flatMap {
      case Succeeded(ioa) => ioa
      case Errored(e)     => IO.raiseError(e)
      case Canceled()     => IO.raiseError(new RuntimeException("Fiber was canceled"))
    }
  }

  val testEx1 = IO("starting").debug >> IO.sleep(1.second) >> IO("done").debug >> IO(42)

  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = {
    val resultIO = for {
      fibA <- ioa.start
      fibB <- iob.start
      resA <- fibA.join
      resB <- fibB.join
    } yield (resA, resB)

    resultIO.flatMap {
      case (Succeeded(fa), Succeeded(fb)) => for {
          a <- fa
          b <- fb
        } yield (a, b)
      case (Errored(e), _) => IO.raiseError(e)
      case (_, Errored(e)) => IO.raiseError(e)
      case _ => IO.raiseError(new RuntimeException("Some computation canceld"))
    }
  }

  val testEx2 = {
    val io1 = IO.sleep(1.second) >> IO(1).debug
    val io2 = IO.sleep(2.seconds) >> IO(2).debug

    tupleIOs(io1, io2)
  }

  def timeoutIO[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val resultIO = for {
      fib <- io.start
      _ <- (IO.sleep(duration) >> fib.cancel).start
      result <- fib.join
    } yield result

    resultIO.flatMap {
      case Succeeded(fa) => fa
      case Errored(e) => IO.raiseError(e)
      case Canceled() => IO.raiseError(new RuntimeException("Computation canceled"))
    }
  }

  val testEx3 = {
    val io = IO("starting").debug >> IO.sleep(1.second) >> IO("done").debug >> IO(42)
    timeoutIO(io, 5000.millis)
  }

  override def run: IO[Unit] =
    testCancel().debug >>
    testEx1.debug >>
    testEx2.debug >>
    testEx3.debug.void
}
