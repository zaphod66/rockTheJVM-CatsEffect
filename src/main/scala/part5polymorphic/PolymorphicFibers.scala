package part5polymorphic

import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, FiberIO, IO, IOApp, Spawn}

import scala.concurrent.duration._

import utils.debug

object PolymorphicFibers extends IOApp.Simple {

  trait MyGenSpawn[F[_], E] {
    def start[A](fa: F[A]): F[Fiber[F, E, A]]
    def never[A]: F[A]
    def cede: F[Unit]

    def racePair[A, B](fa: F[A], fb: F[B]): F[Either[
      (Outcome[F, E, A], Fiber[F, E, B]),
      (Fiber[F, E, A], Outcome[F, E, B])
    ]]
  }

  trait MySpawn[F[_]] extends MyGenSpawn[F, Throwable]

  val mol: IO[Int] = IO(42)
  val fib: IO[Fiber[IO, Throwable, Int]] = mol.start  // FiberIO[Int]

  val spawnIO: Spawn[IO] = Spawn[IO]

  def ioOnSomeThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- spawnIO.start(io)
    out <- fib.join
  } yield out

  import cats.syntax.functor._
  import cats.syntax.flatMap._

  def fOnSomeThread1[F[_], A](fa: F[A])(using spawn: Spawn[F]): F[Outcome[F, Throwable, A]] = for {
    fib <- spawn.start(fa)
    out <- fib.join
  } yield out

  import cats.effect.syntax.spawn._

  def fOnSomeThread2[F[_], A](fa: F[A])(using spawn: Spawn[F]): F[Outcome[F, Throwable, A]] = for {
    fib <- fa.start
    out <- fib.join
  } yield out

  val molFib_v1: IO[Outcome[IO, Throwable, Int]] = ioOnSomeThread(mol)
  val molFib_v2: IO[Outcome[IO, Throwable, Int]] = fOnSomeThread1(mol)
  val molFib_v3: IO[Outcome[IO, Throwable, Int]] = fOnSomeThread2(mol)

  // --------------------------------------------


  def myRace[F[_], A, B](fa: F[A], fb: F[B])(using spawn: Spawn[F]): F[Either[A, B]] =
    spawn.racePair(fa, fb).flatMap {
      case Left((outA, fibB)) => outA match {
        case Succeeded(fa) => fibB.cancel.flatMap(_ => fa.map(Left(_)))
        case Errored(e)    => fibB.cancel.flatMap(_ => spawn.raiseError(e))
        case Canceled()    => fibB.join.flatMap {
          case Succeeded(fb) => fb.map(Right(_))
          case Errored(e)    => spawn.raiseError(e)
          case Canceled()    => spawn.raiseError(new RuntimeException("Both computation canceled"))
        }
      }
      case Right((fibA, outB)) => outB match {
        case Succeeded(fb) => fibA.cancel.flatMap(_ => fb.map(Right(_)))
        case Errored(e)    => fibA.cancel.flatMap(_ => spawn.raiseError(e))
        case Canceled()    => fibA.join.flatMap {
          case Succeeded(fa) => fa.map(Left(_))
          case Errored(e)    => spawn.raiseError(e)
          case Canceled()    => spawn.raiseError(new RuntimeException("Both computation canceled"))
        }
      }
    }

  val fast: IO[String] = IO.sleep(100.millis).onCancel(IO("fast canceled").debug.void) *> IO("fast").debug
  val slow: IO[String] = IO.sleep(200.millis).onCancel(IO("slow canceled").debug.void) *> IO("slow").debug

  val race: IO[Either[String, String]] = myRace(fast, slow)

  override def run: IO[Unit] =
    IO("Polymorphic Fibers").debug *>
    IO("1-----------------------").debug *>
    race.debug *>
    IO("2-----------------------").debug *>
    IO.unit
}
