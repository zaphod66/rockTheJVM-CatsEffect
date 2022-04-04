package part3concurrency

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{IO, IOApp}

import scala.concurrent.duration.*

object RacingIOs extends IOApp.Simple {

  import utils.{debug, runWithSleep}

  val v1: IO[Int] = runWithSleep(42, 1.second)
  val v2: IO[String] = runWithSleep("Name", 2.seconds)

  def testRace: IO[String] = {
    val first = IO.race(v1, v2)

    first.map {
      case Left(l) => s"left won: $l"
      case Right(r) => s"right won: $r"
    }
  }

  def testRacePair: IO[Any] = {
    val raceResult = IO.racePair(v1, v2) // IO[Either[(OutcomeIO[Int], FiberIO[String]), (FiberIO[Int], OutcomeIO[String])]]

    raceResult.flatMap {
      case Left((outV1, fibV2)) => fibV2.cancel *> IO(s"left won").debug *> IO(outV1).debug
      case Right((fibV1, outV2)) => fibV1.cancel *> IO(s"right won").debug *> IO(outV2).debug
    }
  }

  def timeout[A](ioa: IO[A], duration: FiniteDuration): IO[A] =
    IO.race(ioa, IO.sleep(duration)).flatMap {
      case Left(value) => IO(value)
      case Right(_)    => IO.raiseError(new RuntimeException("Computation timed out"))
    }

  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    val raceResult = IO.racePair(ioa, iob)
    raceResult.flatMap {
      case Left((_, fibB)) => fibB.join.flatMap {
        case Succeeded(iob) => iob.map(Right(_))
        case Errored(e)   => IO.raiseError(e)
        case Canceled()   => IO.raiseError(new RuntimeException("Computation A got canceled"))
      }

      case Right((fibA, _)) => fibA.join.flatMap {
        case Succeeded(ioa) => ioa.map(Left(_))
        case Errored(e)     => IO.raiseError(e)
        case Canceled()     => IO.raiseError(new RuntimeException("Computation B got canceled"))
      }
    }

  def myRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((outA, fibB)) => outA match {
        case Succeeded(fa) => fibB.cancel *> fa.map(Left(_))
        case Errored(e)    => fibB.cancel *> IO.raiseError(e)
        case Canceled()    => fibB.join.flatMap {
          case Succeeded(fb) => fb.map(Right(_))
          case Errored(e)    => IO.raiseError(e)
          case Canceled()    => IO.raiseError(new RuntimeException("Both computation canceled"))
        }
      }
      case Right((fibA, outB)) => outB match {
        case Succeeded(fb) => fibA.cancel *> fb.map(Right(_))
        case Errored(e)    => fibA.cancel *> IO.raiseError(e)
        case Canceled()    => fibA.join.flatMap {
          case Succeeded(fa) => fa.map(Left(_))
          case Errored(e)    => IO.raiseError(e)
          case Canceled()    => IO.raiseError(new RuntimeException("Both computation canceled"))
        }
      }
    }

  override def run: IO[Unit] = {
    testRace.debug.void *>
      IO("1 -------").debug *>
      testRacePair.void *>
      IO("2 -------").debug *>
      timeout(v1, 500.millis).handleError(_.getMessage).debug *>
      IO("3 -------").debug *>
      timeout(v1, 2.seconds) *>
      IO("4 -------").debug *>
      unrace(v1, v2).debug.void *>
      IO("5 -------").debug *>
      myRace(v1, v2).void
  }
}
