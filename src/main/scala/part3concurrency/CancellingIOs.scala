package part3concurrency

import cats.effect.{IO, IOApp}
import scala.concurrent.duration._

object CancellingIOs extends IOApp.Simple {

  import utils.debug

  val canceledIO: IO[Int] = IO("waiting") *> IO.canceled *> IO(42)  // has result Canceled

  val paymentSystem: IO[String] = (
    IO("Payment running, don't cancel me...").debug
      *> IO.sleep(1.second)
      *> IO("Payment done.").debug)
    .onCancel(IO("MEGA CANCEL OF DOOM!").debug.void)

  val atomicPaymentSystem1: IO[String] = IO.uncancelable(_ => paymentSystem)  // "masking"
  val atomicPaymentSystem2: IO[String] = paymentSystem.uncancelable           // same

  def canceller[T](io: IO[T], duration: FiniteDuration): IO[Unit] = for {
    fiber <- io.start
    _ <- IO.sleep(duration) *> IO("attempting cancellation...").debug *> fiber.cancel
    res <- fiber.join
  } yield ()

  val megaCancel: IO[Unit] = canceller(paymentSystem, 500.millis)
  val noCancel1: IO[Unit] = canceller(atomicPaymentSystem1, 500.millis)
  val noCancel2: IO[Unit] = canceller(atomicPaymentSystem2, 500.millis)

  val inputPassword: IO[String] = IO("Input password:").debug *> IO("(typing password...)").debug *> IO.sleep(1.second) *> IO("securePassword")
  val verifyPassword: String => IO[Boolean] = (pw: String) => IO("verifying").debug *> IO.sleep(1.second) *> IO(pw == "securePassword")

  val authFlow1: IO[Unit] = IO.uncancelable { _ =>
    for {
      pw <- inputPassword.onCancel(IO("Authentication timed out.").debug.void)
      verified <- verifyPassword(pw)
      _ <- if (verified) IO("Auth successful!").debug else IO("Auth failed!").debug
    } yield ()
  }

  val authProg1: IO[Unit] = canceller(authFlow1, 500.millis)

  val authFlow2: IO[Unit] = IO.uncancelable { poll =>
    for {
      pw <- poll(inputPassword).onCancel(IO("Authentication timed out.").debug.void)
      verified <- verifyPassword(pw)
      _ <- if (verified) IO("Auth successful!").debug else IO("Auth failed!").debug
    } yield ()
  }

  val authProg2: IO[Unit] = canceller(authFlow2,  500.millis)
  val authProg3: IO[Unit] = canceller(authFlow2, 1500.millis)

  val threeStepProg: IO[Unit] = {
    val sequence = IO.uncancelable { poll =>
      poll(IO("cancelable 1").debug *> IO.sleep(1.second)) *>
      IO("uncancelable").debug *> IO.sleep(1.second) *>
      poll(IO("cancelable 2").debug *> IO.sleep(1.second))
    }

    canceller(sequence, 1500.millis)
  }

  override def run: IO[Unit] =
    IO("megaCancel----------").debug *>
    megaCancel *>
    IO("noCancel1-----------").debug *>
    noCancel1 *>
    IO("noCancel2-----------").debug *>
    noCancel2 *>
    IO("authProg1 -----------").debug *>
    authProg1 *>
    IO("authProg2 -----------").debug *>
    authProg2 *>
    IO("authProg3 -----------").debug *>
    authProg3 *>
    IO("threeStepProg -----------").debug *>
    threeStepProg *>
    IO.unit
}
