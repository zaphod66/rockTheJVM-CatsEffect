package part5polymorphic

import cats.Defer
import cats.effect.{IO, IOApp, MonadCancel, Sync}
import utils.debug

import java.io.{ BufferedReader, InputStreamReader }

object PolymorphicSync extends IOApp.Simple {

  import cats.syntax.flatMap
  trait MySync[F[_]] extends MonadCancel[F, Throwable] with Defer[F] {
    def delay[A](thunk: => A): F[A]     // "suspension" of a computation - will run on the CE thread pool
    def blocking[A](thunk: => A): F[A]  // will run on a specific blocking thread pool

    // comes from Defer
    // def defer[A](thunk: => F[A]): F[A] = flatMap(delay(thunk))(identity)
  }

  val syncIO: Sync[IO] = Sync[IO]

  val delayedIO: IO[Int] = syncIO.delay {
    println("I'm an effect!")
    42
  }
  val blockedIO: IO[Int] = syncIO.blocking {
    println("loading...")
    Thread.sleep(100)
    42
  }
  val deferredIO: IO[Int] = IO.defer(delayedIO)

  trait Console[F[_]] {
    def println[A](a: A): F[Unit]
    def readLine(): F[String]
  }

  import cats.syntax.functor._

  object Console {
    // real world comment
    // if you need to add a `using for Sync[F]`, it is a good idea to think about specializing for IO!
    // Because, what kind of effect is more powerful than IO?
    def make[F[_]](using sync: Sync[F]): F[Console[F]] = sync.pure((System.in, System.out)).map {
      case (in, out) => new Console[F] {
        override def println[A](a: A): F[Unit] = sync.blocking(out.println(a))
        override def readLine(): F[String] = {
          val br = new BufferedReader(new InputStreamReader(in))
          sync.blocking(br.readLine())  // problem if br.readLine() takes forever. One should use sync.interruptible
        }
      }
    }
  }

  val useConsole: IO[Unit] = for {
    console <- Console.make[IO]
    _       <- console.println("What is your name?")
    name    <- console.readLine()
    _       <- console.println(s"Hello $name!")
  } yield ()

  override def run: IO[Unit] =
    IO("Polymorphic Sync").debug *>
    IO("1-----------------------").debug *>
    useConsole *>
    IO("2-----------------------").debug *>
    IO.unit
}
