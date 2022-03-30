package part3concurrency

import cats.effect.{IO, IOApp, ParallelF, Resource}

import java.io.{File, FileReader}
import java.util.Scanner
import scala.concurrent.duration.*

object Resources extends IOApp.Simple {

  import utils.debug

  case class Connection(url: String) {
    def open(): IO[String]  = IO(s"opening connection to $url").debug
    def close(): IO[String] = IO(s"closing connection to $url").debug
  }

  val conn1: IO[Unit] = for {
    conn <- IO(Connection("google.com"))
    fiber <- (conn.open() *> IO.never).onCancel(conn.close().void).start
    _ <- IO.sleep(1.second) *> fiber.cancel
  } yield ()

  val bracket: IO[Unit] = IO(Connection("google.com"))
    .bracket(conn => conn.open() *> IO.never)(conn => conn.close().void)

  val bracketProg: IO[Unit] = for {
    fiber <- bracket.start
    _ <- IO.sleep(1.second) *> fiber.cancel
  } yield ()

  def openFileScanner(path: String): IO[Scanner] = IO(new Scanner(new FileReader(new File(path))))
  def fileScannerResource(path: String): (Scanner => IO[Unit]) => Resource[IO, Scanner] = Resource.make(IO(s"open file at $path").debug *> openFileScanner(path))

  def readLines(scanner: Scanner): IO[Unit] =
    if (scanner.hasNextLine) IO(scanner.nextLine()).debug *> IO.sleep(100.millis) >> readLines(scanner)
    else IO.unit

  def readFile(path: String): IO[Unit] = IO(s"reading $path").debug *> openFileScanner(path).bracket { scanner =>
    readLines(scanner)
  } (s => IO(s"Closing file at $path").debug *> IO(s.close()).void)

  // Resources are equivalent to brackets

  val acquireResource: IO[String] = IO("Some resource")
  val usingResource: String => IO[String] = s => IO(s"using the string: $s.").debug
  val releaseResource: String => IO[Unit] = s => IO(s"finalizing the string: $s").debug.void

  val resourceBracket: IO[String] = acquireResource.bracket(usingResource)(releaseResource)
  val resourceResource: IO[String] = Resource.make(acquireResource)(releaseResource).use(usingResource)

  def getResourceFromFile(path: String): Resource[IO, Scanner] =
    Resource.make(openFileScanner(path))(scanner => IO(s"Closing file at $path").debug *> IO(scanner.close()).void)

  def resourceReadFile(path: String): IO[Unit] =
    IO(s"reading $path").debug *> getResourceFromFile(path).use { scanner =>
      readLines(scanner)
    }

  def cancelEffect(effect: IO[Unit], duration: FiniteDuration): IO[Unit] = for {
    fiber <- effect.start
    _ <- IO.sleep(duration) *> IO("cancelling!").debug *> fiber.cancel
  } yield ()

  // nested resources
  def connFromConfFile1(path: String): Resource[IO, Connection] =
    Resource.make(IO(s"open file at $path").debug *> openFileScanner(path))(scanner => IO(s"closing file at $path").debug *> IO(scanner.close()))
      .flatMap(scanner => Resource.make(IO(Connection(scanner.nextLine())))(conn => conn.close().void))

  def connFromConfFile2(path: String): Resource[IO, Connection] = for {
    scanner <- Resource.make(IO(s"open file at $path").debug *> openFileScanner(path))(scanner => IO(s"closing file at $path").debug *> IO(scanner.close()))
    conn    <- Resource.make(IO(Connection(scanner.nextLine())))(conn => conn.close().void)
  } yield conn

  val useConnection1: IO[String] = connFromConfFile1("src/main/resources/connection.txt").use(conn => conn.open() *> IO.never)
  val useConnection2: IO[String] = connFromConfFile2("src/main/resources/connection.txt").use(conn => conn.open() *> IO.never)

  override def run: IO[Unit] = {
    val fileName = "src/main/scala/part3concurrency/Resources.scala"
    conn1 *>
      bracketProg *>
      cancelEffect(readFile(fileName), 1.seconds) *>
      cancelEffect(resourceReadFile(fileName), 1.seconds) *>
      cancelEffect(useConnection1.void, 100.millis) *>
      cancelEffect(useConnection2.void, 100.millis)
  }
}
