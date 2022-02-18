package part3concurrecny

import cats.effect.{IO, IOApp, ParallelF}

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

  def readFile(path: String): IO[Unit] = IO(s"reading $path") *> openFileScanner(path).bracket { scanner =>
    def readLines(scanner: Scanner): IO[Unit] =
      if (scanner.hasNextLine) IO(scanner.nextLine()).debug *> IO.sleep(100.millis) >> readLines(scanner)
      else IO.unit

    readLines(scanner)
  } (s => IO(s"Closing file ar $path").debug *> IO(s.close()).void)

  override def run: IO[Unit] = {
    conn1 *> bracketProg *> readFile("src/main/scala/part3concurrecny/Resources.scala")
  }
}
