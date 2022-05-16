package part3concurrency

import cats.effect.{IO, IOApp, Resource}

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*

object BlockingIOs extends IOApp.Simple {

  import utils.debug

  val someSleeps: IO[Unit] = for {
    _ <- IO.sleep(1.second).debug // semantic blocking
    _ <- IO.sleep(1.second).debug
  } yield ()

  val blockingIO: IO[Int] = IO.blocking {  // runs on a different (smart) threadpool
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}]")
    42
  }

  // yielding
  val yieldingIO: IO[Unit] = for {
    _ <- IO("1").debug
    _ <- IO.cede  // signal (hint) to yield control over this thread
    _ <- IO("2").debug
    _ <- IO.cede
    _ <- IO("3").debug
  } yield ()

  val manyCedes1: IO[Int] = (1 to 49).map(IO.pure).reduce(_.debug *> IO.cede *> _.debug)

  val ecResource: Resource[IO, ExecutionContext] = Resource.make(IO(ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4)))) {
    ec => IO(ec.asInstanceOf[ExecutorService].shutdown()) }

  def manyCedes2: IO[Int] = ecResource.use(ec => manyCedes1.evalOn(ec))

  override def run: IO[Unit] =
    IO("someSleeps ------------").debug *>
      someSleeps *>
      IO("blockingIO ------------").debug *>
      blockingIO *>
      IO("yieldingIO ------------").debug *>
      yieldingIO *>
      IO("manyCedes1 ------------").debug *>
      manyCedes1 *>  // cats effect is really smart in optimizing thing to run on the same thread
      IO("manyCedes2 ------------").debug *>
      manyCedes2 *>  // ec is not managed by cats effect, to single IO's are executed on different threads
      IO.unit
}
