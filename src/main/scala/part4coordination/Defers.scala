package part4coordination

import cats.effect.{Deferred, FiberIO, IO, IOApp, OutcomeIO, Ref}
import utils._

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

  val fileParts: List[String] = List("I ", "love S", "cala", " with Cat", "s Effect!<EOF>")

  def notifierWithRef(): IO[Unit] = {

    def downloadFile(contentRef: Ref[IO, String]): IO[Unit] =
      fileParts.map { part =>
        IO(s"[download] got '$part'").debug *> IO.sleep(500.millis) >> contentRef.update(_ + part)
      }.sequence.void

    def notifyComplete(contentRef: Ref[IO, String]): IO[Unit] = for {
      fileContent <- contentRef.get
      _ <- if (fileContent.endsWith("<EOF>")) IO(s"[notifier] Download complete got: '$fileContent'.").debug
           else IO(s"[notifier] downloading ... Got so far '$fileContent'.").debug *> IO.sleep(300.millis) *> notifyComplete(contentRef)  // busy waiting
    } yield ()

    for {
      contentRef <- Ref[IO].of("")
      _ <- IO.both(downloadFile(contentRef), notifyComplete(contentRef))
    } yield ()
  }

  def notifierWithDefer(): IO[Unit] = {

    def downloadFilePart(part: String, contentRef: Ref[IO, String], signal: Deferred[IO, String]): IO[Unit] = for {
      _ <- IO(s"[download] got '$part'").debug
      _ <- IO.sleep(200.millis)
      c <- contentRef.updateAndGet(_ + part)
      _ <- IO(s"[download] got so far '$c'").debug
      _ <- if (c.contains("<EOF>")) IO("[download] done.").debug *> signal.complete(c) else IO.unit
    } yield ()

    def notifyComplete(signal: Deferred[IO, String]): IO[Unit] = for {
      _ <- IO("[notifier] downloading ...").debug
      c <- signal.get
      _ <- IO(s"[notifier] download complete. Got '$c'").debug
    } yield ()

    for {
      contentRef <- Ref[IO].of("")
      signal     <- Deferred[IO, String]
      downloader = fileParts.map(part => downloadFilePart(part, contentRef, signal)).sequence
      _          <- IO.both(downloader, notifyComplete(signal))
    } yield ()
  }

  def alarmCounter(limit: Int): IO[Unit] = {
    def tick(countRef: Ref[IO, Int], signal: Deferred[IO, Unit]): IO[Unit] = for {
      _ <- IO.sleep(200.millis)
      c <- countRef.updateAndGet(_ + 1)
      _ <- IO(s"[tick] incremented counter to $c").debug
      _ <- if (c == limit) IO(s"counter reached $limit").debug *> signal.complete(()).void else tick(countRef, signal)
    } yield ()

    def wait(signal: Deferred[IO, Unit]): IO[Unit] = for {
      _ <- IO("[wait] waiting for completion ...").debug
      _ <- signal.get
      _ <- IO("[wait] time's up.").debug
    } yield ()

    for {
      countRef <- Ref[IO].of(0)
      signal   <- Deferred[IO, Unit]
      _        <- IO.both(tick(countRef, signal), wait(signal))
    } yield ()
  }

  type RaceResult[A, B] = Either[
    (OutcomeIO[A], FiberIO[B]), // winner result, loser fiber
    (FiberIO[A], OutcomeIO[B])  // loser fiber, winner result
  ]

  type EitherOutcome[A, B] = Either[OutcomeIO[A], OutcomeIO[B]]

  def myRacePair[A, B](ioa: IO[A], iob: IO[B]): IO[RaceResult[A, B]] = IO.uncancelable { poll =>
    for {
      signal <- Deferred[IO, EitherOutcome[A, B]]
      fibA   <- ioa.guaranteeCase(outcomeA => signal.complete(Left(outcomeA)).void).start
      fibB   <- iob.guaranteeCase(outcomeB => signal.complete(Right(outcomeB)).void).start
      result <- poll(signal.get).onCancel { // blocking call should be cancelable
        for {
          cancelFibA <- fibA.cancel.start
          cancelFibB <- fibB.cancel.start
          _ <- cancelFibB.join
          _ <- cancelFibA.join
        } yield ()
      }
    } yield result match {
      case Left(outcomeA)  => Left((outcomeA, fibB))
      case Right(outcomeB) => Right((fibA, outcomeB))
    }
  }

  // -------------------

  val io1: IO[Int] = runWithSleep(42, 1.second)
  val io2: IO[String] = runWithSleep("Name", 500.millis)

  def runMyRacePair[A, B](ioa: IO[A], iob: IO[B]): IO[OutcomeIO[A] | OutcomeIO[B]] = {
    val raceResult = myRacePair(ioa, iob)

    raceResult.flatMap {
      case Left((outA, fibB))  => fibB.cancel *> IO(s"left won").debug *> IO(outA)
      case Right((fibA, outB)) => fibA.cancel *> IO(s"right won").debug *> IO(outB)
    }
  }

  override def run: IO[Unit] =
    IO("Defers").debug *>
    IO("1-----------").debug *>
    demoDeferred() *>
    IO("2-----------").debug *>
    notifierWithRef() *>
    IO("3-----------").debug *>
    notifierWithDefer() *>
    IO("4-----------").debug *>
    alarmCounter(4) *>
    IO("5-----------").debug *>
    runMyRacePair(io1, io2).debug *>
    IO("6-----------").debug *>
    timedRun(runMyRacePair(io1, io2), 100.millis).debug *>
    IO("7-----------").debug *>
    IO.unit
}
