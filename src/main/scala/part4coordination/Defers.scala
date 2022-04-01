package part4coordination

import cats.effect.{Deferred, IO, IOApp, Ref}
import utils.debug

import scala.concurrent.duration.*

object Defers extends IOApp.Simple {

  def demoDeferred(): IO[Unit] = {
    def consumer(signal: Deferred[IO, Int]) = for {
      _ <- IO("[consumer] waiting for result...").debug
      v <- signal.get
      _ <- IO(s"[consumer] got the result: $v").debug
    } yield ()

    val v = 42
    def producer(signal: Deferred[IO, Int]) = for {
      _ <- IO("[producer] Crunching numbers ...").debug
      _ <- IO.sleep(500.millis)
      _ <- signal.complete(v)
      _ <- IO(s"[producer] complete with ($v).").debug
    } yield ()

    for {
      s <- Deferred[IO, Int]
      _ <- IO.both(consumer(s), producer(s))
    } yield ()
  }

  import cats.syntax.traverse._

  def notifierWithRef(): IO[Unit] = {
    val fileParts = List("I ", "love S", "cala", " with Cat", "s Effect!<EOF>")

    def downloadFile(contentRef: Ref[IO, String]): IO[Unit] =
      fileParts.map { part =>
        IO(s"[download] got '$part'").debug *> IO.sleep(1.second) >> contentRef.update(_ + part)
      }.sequence.void

    def notifyComplete(contentRef: Ref[IO, String]): IO[Unit] = for {
      fileContent <- contentRef.get
      _ <- if (fileContent.endsWith("<EOF>")) IO(s"[notifier] Download complete got: '$fileContent'.").debug
           else IO(s"[notifier] downloading ... Got so far '$fileContent'.").debug *> IO.sleep(600.millis) *> notifyComplete(contentRef)  // busy waiting
    } yield ()

    for {
      contentRef <- Ref[IO].of("")
      _ <- IO.both(downloadFile(contentRef), notifyComplete(contentRef))
    } yield ()
  }

  override def run: IO[Unit] =
    IO("Defers").debug *>
    IO("1-----------").debug *>
    demoDeferred() *>
    IO("2-----------").debug *>
    notifierWithRef() *>
    IO("3-----------").debug *>
    IO.unit
}
