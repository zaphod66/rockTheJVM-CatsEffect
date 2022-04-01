package part4coordination

import cats.effect.{IO, IOApp}

import utils.debug

object Defers extends IOApp.Simple {

  override def run: IO[Unit] =
    IO("Defers").debug *>
    IO("1-----------").debug *>
    IO.unit
}
