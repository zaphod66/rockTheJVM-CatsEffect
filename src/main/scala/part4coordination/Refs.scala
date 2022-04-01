package part4coordination

import cats.effect.{IO, IOApp, Ref}
import utils.debug

object Refs extends IOApp.Simple {

  val atomicRef_1: IO[Ref[IO, Int]] = Ref[IO].of(42)
  val atomicRef_2: IO[Ref[IO, Int]] = IO.ref(42)

  // modifying
  val modified_1: IO[IO[Unit]] = atomicRef_1.map(ref => ref.set(43) ) // threadSafe
  val modified_2: IO[Int] = atomicRef_1.flatMap(ref => ref.getAndSet(43) )
  val modified_3: IO[Unit] = atomicRef_1.flatMap(ref => ref.update(_ * 10))
  val modified_4: IO[Int] = atomicRef_1.flatMap(ref => ref.getAndUpdate(_ + 1)) // get old value
  val modified_5: IO[Int] = atomicRef_1.flatMap(ref => ref.updateAndGet(_ + 3)) // get new value
  val modified_6: IO[String] = atomicRef_1.flatMap(ref => ref.modify(v => (v + 4, s"current value is $v")))

  // getting
  val value_0: IO[Int] = atomicRef_1.flatMap( ref => ref.get )

  import cats.syntax.parallel._

  def concurrentWordCountImpure(list: List[String]): IO[Int] = {
    var count = 0

    def task(workload: String): IO[Int] = {
      val wordCount = workload.split(" ").length

      for {
        _        <- IO(s"Computing words for '$workload' : $wordCount").debug
        newCount <- IO(count + wordCount)
        _        <- IO(s"new total: $newCount").debug
        _        <- IO(count += newCount)
      } yield newCount
    }

    list.map(task).parSequence.map(_.max)
  }

  def concurrentWordCountPure(list: List[String]): IO[Int] = {
    def task(workload: String, total: Ref[IO, Int]): IO[Int] = {
      val wordCount = workload.split(" ").length

      for {
        _        <- IO(s"Computing words for '$workload' : $wordCount").debug
        newCount <- total.updateAndGet( currentCount => currentCount + wordCount)
        _        <- IO(s"new total: $newCount").debug
      } yield newCount
    }

    for {
      count <- Ref[IO].of(0)
      total <- list.map(s => task(s, count)).parSequence
    } yield total.max
  }

  val list = List("I love cats effect", "This ref thing is useless", "This is a lot of code")

  import scala.concurrent.duration._

  def ticklingClockImpure(): IO[Unit] = {
    var ticks: Long = 0L

    def tickingClock: IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ <- IO(ticks += 1)
      _ <- tickingClock
    } yield ()

    def printTicks: IO[Unit] = for {
      _ <- IO.sleep(5.seconds)
      _ <- IO(s"TICKS: $ticks").debug
      _ <- printTicks
    } yield ()

    for {
      _ <- (tickingClock, printTicks).parTupled
    } yield ()
  }

  def ticklingClockPure(): IO[Unit] = {

    def tickingClock(ticks: Ref[IO, Long]): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ <- ticks.update(_ + 1)
      _ <- tickingClock(ticks)
    } yield ()

    def printTicks(ticks: Ref[IO, Long]): IO[Unit] = for {
      _ <- IO.sleep(5.seconds)
      t <- ticks.get
      _ <- IO(s"TICKS: $t").debug
      _ <- printTicks(ticks)
    } yield ()


    for {
      ticks <- IO.ref(0L)
      _     <- (tickingClock(ticks), printTicks(ticks)).parTupled
    } yield ()
  }

  def timedRunner(io: IO[Unit], duration: FiniteDuration): IO[Unit] = {
    for {
      fib <- io.start
      _   <- IO.sleep(duration) *> fib.cancel
      _   <- fib.join
    } yield ()
  }

  override def run: IO[Unit] =
    IO("Refs").debug *>
    IO("1-----------").debug *>
    concurrentWordCountImpure(list).debug *>
    IO("2-----------").debug *>
    concurrentWordCountPure(list).debug *>
    IO("3-----------").debug *>
    timedRunner(ticklingClockImpure(), 11.seconds) *>
    IO("4-----------").debug *>
    timedRunner(ticklingClockPure(), 11.seconds) *>
    IO("4-----------").debug *>
    IO.unit
}
