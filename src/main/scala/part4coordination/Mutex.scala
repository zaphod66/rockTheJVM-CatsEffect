package part4coordination

import cats.effect.Outcome.{ Canceled, Errored, Succeeded }
import cats.effect.{Deferred, IO, IOApp, Ref}
import cats.syntax.parallel.*

import scala.collection.immutable.Queue
import scala.concurrent.duration.*

abstract class Mutex {
  def acquire: IO[Unit]
  def release: IO[Unit]
}

object Mutex {
  type Signal = Deferred[IO, Unit]
  case class State(locked: Boolean, waiting: Queue[Signal])
  val unlocked: State = State(locked = false, Queue.empty[Signal])

  def createSignal(): IO[Signal] = Deferred[IO, Unit]

  def create: IO[Mutex] = Ref[IO].of(unlocked).map(createMutexWithCancel)

  def createMutexWithoutCancel(state: Ref[IO, State]): Mutex =
    new Mutex {
      /*
        - if the mutex is unlocked, state becomes (true, [])
        - if the mutex is locked, state becomes (true, queue + new signal)
      */
      override def acquire: IO[Unit] = createSignal().flatMap { signal =>
        state.modify {
          case State(false, _) => (State(locked = true, Queue.empty[Signal]), IO.unit)
          case State(true, q) => (State(locked = true, q.enqueue(signal)), signal.get)
        }.flatten
      }

      /*
        - if mutex is unlocked, leave the state unchanged
        - if mutex is locked,
          - if queue is empty, unlock mutex => (false, [])
          - if queue is not empty, take a signal out of the queue and complete it => (true, queue - signal)
      */
      override def release: IO[Unit] = state.modify {
        case State(false, _) => (unlocked, IO.unit)
        case State(true, q) =>
          if (q.isEmpty) (unlocked, IO.unit)
          else {
            val (s1, q1) = q.dequeue
            (State(true, q1), s1.complete(()).void)
          }
      }.flatten
    }

  def createMutexWithCancel(state: Ref[IO, State]): Mutex =
    new Mutex {
      override def acquire: IO[Unit] = IO.uncancelable { poll =>
        createSignal().flatMap { signal =>

          val cleanup = state.modify {
            case State(locked, q0) =>
              val q1 = q0.filterNot(_ eq signal)
              (State(locked, q1), release)
          }.flatten

          state.modify {
            case State(false, _) => (State(locked = true, Queue.empty[Signal]), IO.unit)
            case State(true, q) => (State(locked = true, q.enqueue(signal)), poll(signal.get).onCancel(cleanup))
          }.flatten
        }
      }

      override def release: IO[Unit] = state.modify { // release shouldn't be cancelable and modify is already atomic
        case State(false, _) => (unlocked, IO.unit)
        case State(true, q) =>
          if (q.isEmpty) (unlocked, IO.unit)
          else {
            val (s1, q1) = q.dequeue
            (State(true, q1), s1.complete(()).void)
          }
      }.flatten
    }
}

object MutexRunner extends IOApp.Simple {

  import utils.debug

  def criticalTask(v: Int): IO[Int] = IO.sleep(v.millis) *> IO(v)

  def createNonLockingTask(id: Int): IO[Int] = for {
    _   <- IO(s"[task $id] working...").debug
    res <- criticalTask(id * 100)
    _   <- IO(s"[task $id] ... got result $res").debug
  } yield res

  def runNonLockingTasks(): IO[List[Int]] = (1 to 10).toList.parTraverse(id => createNonLockingTask(id))

  def createLockingTask(id: Int, mutex: Mutex): IO[Int] = for {
    _   <- IO(s"[task $id] waiting for permission...").debug
    _   <- mutex.acquire
    _   <- IO(s"[task $id] working...").debug
    res <- criticalTask(id * 100)
    _   <- IO(s"[task $id] ... got result $res").debug
    _   <- mutex.release
    _   <- IO(s"[task $id] lock released.").debug
  } yield res

  def runLockingTasks(): IO[List[Int]] = for {
    mut <- Mutex.create
    res <- (1 to 10).toList.parTraverse(id => createLockingTask(id, mut))
  } yield res

  def createCancellingTask(id: Int, mutex: Mutex): IO[Int] = {
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

  def runCancellingTasks(): IO[List[Int]] = for {
    mut <- Mutex.create
    res <- (1 to 10).toList.parTraverse(id => createCancellingTask(id, mut))
  } yield res

  override def run: IO[Unit] =
    IO("Mutex").debug *>
      IO("1-----------").debug *>
      runNonLockingTasks().debug *>
      IO("2-----------").debug *>
      runLockingTasks().debug *>
      IO("3-----------").debug *>
      runCancellingTasks().debug *>
      IO("4-----------").debug *>
      IO.unit
}
