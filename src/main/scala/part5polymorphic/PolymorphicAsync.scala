package part5polymorphic

import cats.effect.{Async, Concurrent, IO, IOApp, Sync, Temporal}
import utils.general.debug

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import java.util.concurrent.{ExecutorService, Executors}
object PolymorphicAsync extends IOApp.Simple {

  trait MyAsync[F[_]] extends Sync[F] with Temporal[F] {
    def executionContext: F[ExecutionContext]
    def async[A](cb: (Either[Throwable, A] => Unit) => F[Option[F[Unit]]]): F[A]
    def evalOn[A](fa: F[A], ec: ExecutionContext): F[A]

    def never[A]: F[A] = async_[A] { _ => () }
    def async_[A](cbo: (Either[Throwable, A] => Unit) => Unit): F[A] =
      async(cbi => map(pure(cbo(cbi)))(_ => None))
  }

  val asyncIO: Async[IO] = Async[IO]

  val ec: IO[ExecutionContext] = asyncIO.executionContext

  // ===========

  val threadPool: ExecutorService = Executors.newFixedThreadPool(10)
  type Callback[A] = Either[Throwable, A] => Unit
  val asyncMOL1: IO[Int] = asyncIO.async_ { (cb: Callback[Int]) =>
    threadPool.execute { () =>
      println(s"[${Thread.currentThread().getName}] computing MOL")
      cb(Right(42))
    }
  }

  val asyncMOL2: IO[Int] = asyncIO.async { (cb: Callback[Int]) =>
    IO {
      threadPool.execute { () =>
        println(s"[${Thread.currentThread().getName}] computing MOL")
        cb(Right(42))
      }
    }.as(None)
  }

  val myEC: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(threadPool)
  val asyncMOL3: IO[Int] = asyncIO.evalOn(IO("Computing MOL").debug *> IO(42), myEC)

  // ===========

  def effect1[F[_]: Concurrent, A](a: A): F[A] = Concurrent[F].pure(a)
  def effect2[F[_]: Sync, A](a: A): F[A] = Sync[F].pure(a)

  import cats.syntax.functor._
  import cats.syntax.flatMap._

  // Async reconciles the conflicting context bounds of Sync and Concurrent
  def tupledEffect[F[_]: Async, A](a: A): F[(A, A)] = for {
    fst <- effect1(a)
    snd <- effect2(a)
  } yield (fst, snd)

  override def run: IO[Unit] =
    IO("PolymorphicAsync").debug *>
    IO("1---------------").debug *>
    asyncMOL1.debug *>
    IO("2---------------").debug *>
    asyncMOL2.debug *>
    IO("3---------------").debug *>
    asyncMOL3.debug *>
    IO("4---------------").debug *>
    IO(threadPool.shutdown()) *>
    IO.unit
}
