package part2effects

import cats.effect.{IO, IOApp, ExitCode}

import scala.io.StdIn

object IOApps {

  val prog: IO[Unit] = for {
    line <- IO(StdIn.readLine())
    _    <- IO(println(s"Hello $line!"))
  } yield ()
}

object TestApp {
  import IOApps.prog

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global

    prog.unsafeRunSync()
  }
}

object CEApp1 extends IOApp {
  import IOApps.prog

  override def run(args: List[String]): IO[ExitCode] = prog.as(ExitCode.Success)
}

object CEApp2 extends IOApp.Simple {
  import IOApps.prog

  override def run: IO[Unit] = prog
}
