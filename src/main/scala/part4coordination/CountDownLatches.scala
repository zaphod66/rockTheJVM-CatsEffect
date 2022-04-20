package part4coordination

import cats.effect.std.CountDownLatch
import cats.effect.{IO, IOApp, Resource}
import cats.syntax.parallel.*
import cats.syntax.traverse.*

import scala.concurrent.duration.*
import utils.debug

import java.io.{File, FileWriter}
import scala.io.Source
import scala.util.Random

object CountDownLatches extends IOApp.Simple {

  def starter(latch: CountDownLatch[IO], num: Int): IO[Unit] = for {
    _ <- IO("Starting...").debug
    _ <- (num to 1 by -1).toList.traverse { i =>
      for {
        _ <- IO(s"$i ...").debug *> IO.sleep(500.millis)
        _ <- latch.release
      } yield ()
    }
    _ <- IO("Go!").debug
  } yield ()

  def worker(id: Int, latch: CountDownLatch[IO]): IO[Unit] = for {
    _ <- IO(s"[worker $id] waiting for signal...").debug
    _ <- latch.await
    _ <- IO(s"[worker $id] working...").debug
  } yield ()

  val num = 5

  def runner(): IO[Unit] = for {
    latch <- CountDownLatch[IO](num)
    starterFib <- starter(latch, num).start
    _ <- (1 to 10).toList.parTraverse(id => worker(id, latch))
    _ <- starterFib.join
  } yield ()

  // simulate file downloader

  object FileServer {
    val fileChunks: Array[String] = Array(
      "I love Scal",
      "a. Cats Effects ar",
      "e cool! This is the last ch",
      "unk. And here are som",
      "e more amend",
      "ments!",
      " This was cool!"
    )

    def getNumChunks: IO[Int] = IO(fileChunks.length)
    def getFileChunk(n: Int): IO[String] = IO(fileChunks(n))
  }

  def writeToFile(path: String, content: String): IO[Unit] = {
    val fileResources = Resource.make(IO(new FileWriter(new File(path))))(writer => IO(writer.close()))

    fileResources.use { writer =>
      IO(writer.write(content))
    }
  }

  def appendFileContents(fromPath: String, toPath: String): IO[Unit] = {
    val resources = for {
      reader <- Resource.make(IO(Source.fromFile(fromPath)))(source => IO(source.close()))
      writer <- Resource.make(IO(new FileWriter(new File(toPath), true)))(writer => IO(writer.close()))
    } yield (reader, writer)

    resources.use {
      case (r, w) => IO(r.getLines().foreach(w.write))
    }
  }

  def downloadChunk(id: Int, latch: CountDownLatch[IO], fileName: String, destFolder: String): IO[Unit] = for {
    _ <- IO(s"[task $id] downloading chunk").debug
    d = Random.nextInt(1000)
    _ <- IO(s"[task $id] downloading for $d ms.").debug
    _ <- IO.sleep(d.millis)
    content <- FileServer.getFileChunk(id)
    _ <- writeToFile(s"$destFolder/$fileName.part$id", content)
    _ <- IO(s"[task $id] chunk downloaded!").debug
    _ <- latch.release
  } yield ()

  def downloadFile(fileName: String, destFolder: String): IO[Unit] = for {
      num   <- FileServer.getNumChunks
      latch <- CountDownLatch[IO](num)
      _     <- IO(s"Download started on $num fibers").debug
      _     <- (0 until num).toList.parTraverse(id => downloadChunk(id, latch, fileName, destFolder))
      _     <- latch.await
      _     <- (0 until num).toList.traverse { i => appendFileContents(s"$destFolder/$fileName.part$i", s"$destFolder/$fileName")}
    } yield ()

  override def run: IO[Unit] =
    IO("CountDownLatches").debug *>
    IO("1----------").debug *>
    runner() *>
    IO("2----------").debug *>
    downloadFile("download.txt", "src/main/resources/") *>
    IO("3----------").debug *>
    IO.unit
}
