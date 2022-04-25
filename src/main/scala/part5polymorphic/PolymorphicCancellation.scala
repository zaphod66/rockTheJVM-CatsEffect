package part5polymorphic

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.{Applicative, Monad}
import cats.effect.{IO, IOApp, MonadCancel, Poll}
import utils.debug

import scala.concurrent.duration._

object PolymorphicCancellation extends IOApp.Simple {

  trait MyApplicativeError[F[_], E] extends Applicative[F] {
    def raiseError[A](error: E): F[A]
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
  }

  trait MyMonadError[F[_], E] extends MyApplicativeError[F, E] with Monad[F]

  trait MyPoll[F[_]] {
    def apply[A](fa: F[A]): F[A]
  }

  trait MyMonadCancel[F[_], E] extends MyMonadError[F, E] {
    def canceled: F[Unit]
    def uncancelable[A](body: Poll[F] => F[A]): F[A]
  }

  val monadCancelIO: MonadCancel[IO, Throwable] = MonadCancel[IO]

  val meaningOfLive: IO[Int] = monadCancelIO.pure(42)
  val advancedMol: IO[Int] = monadCancelIO.map(meaningOfLive)(_ * 2)

  val mustCompute_v1: IO[Int] = monadCancelIO.uncancelable { _ =>
    for {
      _ <- monadCancelIO.pure("Once started, will finish")
      r <- monadCancelIO.pure(56)
    } yield r
  }

  import cats.syntax.functor._
  import cats.syntax.flatMap._

  def mustComputeGeneral[F[_], E](using mc: MonadCancel[F, E]): F[Int] = mc.uncancelable { _ =>
    for {
      _ <- mc.pure("Once started, will finish")
      r <- mc.pure(56)
    } yield r
  }

  val mustCompute_v2: IO[Int] = mustComputeGeneral[IO, Throwable]

  val mustComputeListener_v1: IO[Int] = mustCompute_v1.onCancel(IO("Canceled!").void)
  val mustComputeListener_v2: IO[Int] = monadCancelIO.onCancel(mustCompute_v1, IO("Canceled!").void)

  val computationFinalizer: IO[Int] = monadCancelIO.guaranteeCase(IO(42)) {
    case Succeeded(fa) => fa.map(v => s"successful: $v").void
    case Errored(e)    => IO(s"failed: $e").void
    case Canceled()    => IO("canceled").void
  }

  val computationUsage: IO[String] = monadCancelIO.bracket(IO(42)) { value =>
    IO(s"using meaning of live: $value")
  } { value =>
    IO(s"releasing $value")
  }

  // --------------------------

  import utils.general.debug

  def unsafeSleep[F[_], E](duration: FiniteDuration)(using mc: MonadCancel[F, E]): F[Unit] =
    mc.pure(Thread.sleep(duration.toMillis))

  def canceller[T](io: IO[T], duration: FiniteDuration): IO[Unit] = for {
    fiber <- io.start
    _ <- IO.sleep(duration) *> IO("attempting cancellation...").debug *> fiber.cancel
    _ <- fiber.join
  } yield ()

  def inputPassword[F[_], E](using mc: MonadCancel[F, E]): F[String] = for {
    _ <- mc.pure("Input password:").debug
    _ <- mc.pure("(typing password...)").debug
    _ <- unsafeSleep[F, E](1.second)
    p <- mc.pure("securePassword")
  } yield p

  def verifyPassword[F[_], E](pw: String)(using mc: MonadCancel[F, E]): F[Boolean] = for {
    _ <- mc.pure("verifying").debug
    _ <- unsafeSleep[F, E](1.second)
  } yield pw == "securePassword"

  import cats.effect.syntax.monadCancel._

  def authFlow[F[_], E](using mc: MonadCancel[F, E]): F[Unit] = mc.uncancelable { poll =>
    for {
      pw <- poll(inputPassword).onCancel(mc.pure("Authentication timed out.").debug.void)
      verified <- verifyPassword(pw)
      _ <- if (verified) mc.pure("Auth successful!").debug else mc.pure("Auth failed!").debug
    } yield ()
  }

  val authProg2: IO[Unit] = canceller(authFlow[IO, Throwable],  500.millis)
  val authProg3: IO[Unit] = canceller(authFlow[IO, Throwable], 1500.millis)

  override def run: IO[Unit] =
    IO("Polymorphic Cancellation").debug *>
    IO("1-----------------------").debug *>
    authProg2 *>
    IO("2-----------------------").debug *>
    authProg3 *>
    IO("3-----------------------").debug *>
    IO.unit
}
