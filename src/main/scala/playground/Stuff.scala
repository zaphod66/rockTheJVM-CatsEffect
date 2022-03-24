package playground

import cats.effect.{IO, IOApp}

object Stuff extends IOApp.Simple {

  override def run: IO[Unit] =
    IO.println("rockTheJVM-CatsEffect")
}
