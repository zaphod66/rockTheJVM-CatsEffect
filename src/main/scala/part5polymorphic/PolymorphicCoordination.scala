package part5polymorphic

import cats.effect.{Concurrent, Deferred, Fiber, IO, IOApp, Outcome, Ref, Spawn}
import cats.effect.Outcome.{ Canceled, Errored, Succeeded }

import utils.runWithSleep

import scala.collection.immutable.Queue
import scala.concurrent.duration.*

abstract class MyMutex[F[_]] {
  def acquire: F[Unit]
  def release: F[Unit]
}

object MyMutex {
  import cats.syntax.functor._
  import cats.syntax.flatMap._
  import cats.effect.syntax.monadCancel._

  type Signal[F[_]] = Deferred[F, Unit]
  case class State[F[_]](locked: Boolean, waiting: Queue[Signal[F]])
  def unlocked[F[_]]: State[F] = State[F](locked = false, Queue.empty[Signal[F]])

  def createSignal[F[_]]()(using concurrent: Concurrent[F]): F[Signal[F]] = concurrent.deferred[Unit]

  def create[F[_]](using concurrent: Concurrent[F]): F[MyMutex[F]] = concurrent.ref(unlocked).map(createMutexWithCancel)

  def createMutexWithCancel[F[_]](state: Ref[F, State[F]])(using concurrent: Concurrent[F]): MyMutex[F] =
    new MyMutex[F] {
      override def acquire: F[Unit] = concurrent.uncancelable { poll =>
        createSignal().flatMap { signal =>

          val cleanup = state.modify {
            case State(locked, q0) =>
              val q1 = q0.filterNot(_ eq signal)
              (State(locked, q1), release)
          }.flatten

          state.modify {
            case State(false, _) => (State[F](locked = true, Queue.empty[Signal[F]]), concurrent.unit)
            case State(true, q) => (State[F](locked = true, q.enqueue(signal)), poll(signal.get).onCancel(cleanup))
          }.flatten
        }
      }

      override def release: F[Unit] = state.modify { // release shouldn't be cancelable and modify is already atomic
        case State(false, _) => (unlocked[F], concurrent.unit)
        case State(true, q) =>
          if (q.isEmpty) (unlocked[F], concurrent.unit)
          else {
            val (s1, q1) = q.dequeue
            (State(true, q1), s1.complete(()).void)
          }
      }.flatten
    }
}

object PolymorphicCoordination extends IOApp.Simple {

  trait MyConcurrent[F[_]] extends Spawn[F] {
    def ref[A](a: A): F[Ref[F, A]]
    def deferred[A]: F[Deferred[F, A]]
  }

  val concurrentIO: Concurrent[IO] = Concurrent[IO]
  val aDeferred1: IO[Deferred[IO, Int]] = Deferred[IO, Int]  // given Concurrent[IO] in scope
  val aDeferred2: IO[Deferred[IO, Int]] = concurrentIO.deferred[Int]

  // -----------------------------------------

  import cats.syntax.functor._
  import cats.syntax.flatMap._

  import utils.general._

  def alarmCounter[F[_]](limit: Int)(using concurrent: Concurrent[F]): F[Unit] = {
    def tick(countRef: Ref[F, Int], signal: Deferred[F, Unit]): F[Unit] = for {
      _ <- unsafeSleep(200.millis)
      c <- countRef.updateAndGet(_ + 1)
      _ <- concurrent.pure(s"[tick] incremented counter to $c").debug
      _ <- if (c >= limit) concurrent.pure(s"[tick] counter reached $limit").debug.flatMap(_ => signal.complete(()).void) else tick(countRef, signal)
    } yield ()

    def wait(signal: Deferred[F, Unit]): F[Unit] = for {
      _ <- concurrent.pure("[wait] waiting for completion ...").debug
      _ <- signal.get
      _ <- concurrent.pure("[wait] time's up.").debug
    } yield ()

    for {
      countRef <- concurrent.ref(0)
      signal   <- concurrent.deferred[Unit]
      _        <- concurrent.both(tick(countRef, signal), wait(signal))
    } yield ()
  }

  // -----------------------------------------

  type RaceResult[F[_], A, B] = Either[
    (Outcome[F, Throwable, A], Fiber[F, Throwable, B]), // winner result, loser fiber
    (Fiber[F, Throwable, A], Outcome[F, Throwable, B])  // loser fiber, winner result
  ]

  type EitherOutcome[F[_], A, B] = Either[Outcome[F, Throwable, A], Outcome[F, Throwable, B]]

  import cats.effect.syntax.spawn._
  import cats.effect.syntax.monadCancel._

  def myRacePair[F[_], A, B](fa: F[A], fb: F[B])(using concurrent: Concurrent[F]): F[RaceResult[F, A, B]] = concurrent.uncancelable { poll =>
    for {
      signal <- concurrent.deferred[EitherOutcome[F, A, B]]
      fibA   <- fa.guaranteeCase(outcomeA => signal.complete(Left(outcomeA)).void).start
      fibB   <- fb.guaranteeCase(outcomeB => signal.complete(Right(outcomeB)).void).start
      result <- poll(signal.get).onCancel { // blocking call should be cancelable
        for {
          cancelFibA <- fibA.cancel.start
          cancelFibB <- fibB.cancel.start
          _ <- cancelFibB.join
          _ <- cancelFibA.join
        } yield ()
      }
    } yield result match {
      case Left(outcomeA)  => Left((outcomeA, fibB))
      case Right(outcomeB) => Right((fibA, outcomeB))
    }
  }

  val io1: IO[Int] = runWithSleep(42, 1.second)
  val io2: IO[String] = runWithSleep("Name", 500.millis)

  def runMyRacePair[A, B](ioa: IO[A], iob: IO[B]): IO[Outcome[IO, Throwable, A] | Outcome[IO, Throwable, B]] = {
    val raceResult = myRacePair(ioa, iob)

    raceResult.flatMap {
      case Left((outA, fibB))  => fibB.cancel *> IO(s"left won").debug *> IO(outA)
      case Right((fibA, outB)) => fibA.cancel *> IO(s"right won").debug *> IO(outB)
    }
  }

  // -----------------------------------------

  import utils.debug

  def criticalTask(v: Int): IO[Int] = IO.sleep(v.millis) *> IO(v)

  def createLockingTask(id: Int, mutex: MyMutex[IO]): IO[Int] = for {
    _   <- IO(s"[task $id] waiting for permission...").debug
    _   <- mutex.acquire
    _   <- IO(s"[task $id] working...").debug
    res <- criticalTask(id * 100)
    _   <- IO(s"[task $id] ... got result $res").debug
    _   <- mutex.release
    _   <- IO(s"[task $id] lock released.").debug
  } yield res

  def createCancellingTask(id: Int, mutex: MyMutex[IO]): IO[Int] = {
    if (id % 2 == 0) createLockingTask(id, mutex)
    else for {
      fib <- createLockingTask(id, mutex).onCancel(IO(s"[task $id] got cancelled").debug.void).start
      _   <- IO.sleep(500.millis) *> fib.cancel
      out <- fib.join
      res <- out match {
        case Succeeded(effect) => effect
        case Errored(_)        => IO(-1)
        case Canceled()        => IO(-2)
      }
    } yield res
  }

  import cats.syntax.parallel._

  def runCancellingTasks(): IO[List[Int]] = for {
    mut <- MyMutex.create[IO]
    res <- (1 to 10).toList.parTraverse(createCancellingTask(_, mut))
  } yield res

  // -----------------------------------------

  override def run: IO[Unit] =
    IO("Polymorphic Coordination").debug *>
    IO("1-----------------------").debug *>
    alarmCounter[IO](3) *>
    IO("2-----------------------").debug *>
    runMyRacePair(io1, io2) *>
    IO("3-----------------------").debug *>
    runCancellingTasks().debug *>
    IO("4-----------------------").debug *>
    IO.unit
}
