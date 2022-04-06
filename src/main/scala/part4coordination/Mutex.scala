package part4coordination

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

  def create: IO[Mutex] = Ref[IO].of(unlocked).map { state /* (state: Ref[IO, State]) */ =>
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

  override def run: IO[Unit] =
    IO("Mutex").debug *>
      IO("1-----------").debug *>
      runNonLockingTasks().debug *>
      IO("2-----------").debug *>
      runLockingTasks().debug *>
      IO("3-----------").debug *>
      IO.unit
}
