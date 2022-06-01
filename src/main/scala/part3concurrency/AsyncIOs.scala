package part3concurrency

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{FiberIO, IO, IOApp}

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import utils.debug

object AsyncIOs extends IOApp.Simple {

  val threadPool: ExecutorService = Executors.newFixedThreadPool(8)
  given ec: ExecutionContext = ExecutionContext.fromExecutorService(threadPool)

  type Callback[A] = Either[Throwable, A] => Unit

  def computeSomething(id: Int)(): Int = {
    println(s"[${Thread.currentThread().getName}] ($id) computing...")
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] ($id) ...computing")
    42
  }
  def computeSomethingEither(id: Int = 0)(): Either[Throwable, Int] = Try {
    computeSomething(id)()
  }.toEither

  def asyncComputeIO[A](thunk: () => Either[Throwable, A])(using ec: ExecutionContext): IO[A] = IO.async_ { cb =>
    ec.execute { () =>
      val result = thunk()
      cb(result)
    }
  }

  def asyncToIO[A](thunk: () => A)(using ec: ExecutionContext): IO[A] = IO.async_ { cb =>
    ec.execute { () =>
      val result = Try { thunk() }.toEither
      cb(result)
    }
  }

  lazy val computeFuture: Future[Int] = Future { computeSomething(3)() }

  def futureToIO[A](future: => Future[A])(using ec: ExecutionContext): IO[A] = IO.async_ { cb =>
    future.onComplete( tryResult =>
      cb(tryResult.toEither)
    )(ec)
  }

  def never[A]: IO[A] = IO.async_[A] { _ => () }

  import scala.concurrent.duration._

  // FULL ASYNC call (you have also control on cancellation, meaning a finalizer is called)
  def demoAsyncComputation(id: Int, cancel: Boolean): IO[Int] = {
    val asyncIO: IO[Int] = IO.async[Int] { (cb: Callback[Int]) =>
      /*
      - finalizer in case of cancellation
      - finalizers are of type IO[Unit]
      - to be able to not specify => Option[IO[Unit]]
      - creating an Option is an effect => IO[Option[IO[Unit]]]

      - we have to return a IO[Option[IO[Unit]]]
      */

      IO {
        threadPool.execute { () =>
          val result = computeSomethingEither(id)()
          cb(result)
        }
      }.as(Some(IO("Called on cancellation!").debug.void))
    }

    (for {
      fib <- asyncIO.start
      _   <- IO.sleep(500.millis) *> (if (cancel) IO("Cancelling!").debug *> fib.cancel else IO.unit)
      out <- fib.join
    } yield out match {
      case Succeeded(v) => v
      case Errored(_)   => IO(0)
      case Canceled()   => IO(-1)
    }).flatten
  }

  def demoAsync_Computation(id: Int, cancel: Boolean): IO[Int] = {
    (for {
      fib <- asyncToIO(computeSomething(id)).start
      _   <- IO.sleep(500.millis) *> (if (cancel) IO("Cancelling!").debug *> fib.cancel else IO.unit)
      out <- fib.join
    } yield out match {
      case Succeeded(v) => v
      case Errored(_)   => IO(0)
      case Canceled()   => IO(-1)
    }).flatten
  }

  def timed[A](ioa: IO[A]): IO[(FiniteDuration, A)] = for {
    start <- IO.realTime
    a     <- ioa
    end   <- IO.realTime
  } yield (end.minus(start), a)

  override def run: IO[Unit] =
    IO("1---------").debug *>
    asyncComputeIO(computeSomethingEither(1)).debug *>
    IO("2---------").debug *>
    asyncToIO(computeSomething(2)).debug *>
    IO("3---------").debug *>
    futureToIO(computeFuture).debug *>
    IO("4---------").debug *>
    IO.fromFuture(IO(computeFuture)).debug *>
    IO("5---------").debug *>
    demoAsyncComputation(4, true).debug *>
    IO("6---------").debug *>
    demoAsyncComputation(5, false).debug *>
    IO("7---------").debug *>
    timed(demoAsync_Computation(6, true)).debug *>
    IO("8---------").debug *>
    timed(demoAsync_Computation(7, false)).debug *>
//    demoAsync_Computation(7, false).timed.map { case (t, a) => (t.toMillis, a) }.debug *>
    IO("9---------").debug *>
    timed(IO(computeSomething(8)()).timeout(500.millis).handleError(_ => -2)).debug *>
    IO("10--------").debug *>
    IO(threadPool.shutdown())
}
