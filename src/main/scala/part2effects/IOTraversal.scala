package part2effects

import cats.effect.{IO, IOApp}

object IOTraversal extends IOApp.Simple{

  val workload = List("This is string 1", "Another String in the list", "and the final string")

  import utils.debug

  def computeIO(string: String): IO[Int] = IO {
    import scala.util.Random

    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }.debug

  import cats.Traverse
  import cats.instances.list._
  val listTraverse: Traverse[List] = Traverse[List]

  val resultIO: IO[List[Int]] = listTraverse.traverse(workload)(computeIO)

  import cats.syntax.parallel._

  val resultIOPar: IO[List[Int]] = workload.parTraverse(computeIO)

  override def run: IO[Unit] =
    resultIO.map(_.sum).debug.void *>
      IO("------------").debug *>
      resultIOPar.map(_.sum).debug.void
}
