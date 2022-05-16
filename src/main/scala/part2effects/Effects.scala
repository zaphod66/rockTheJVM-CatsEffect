package part2effects

import cats.effect.{IO, IOApp}

object Effects extends IOApp.Simple {

  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun())).unsafeRun()
  }

  val io: MyIO[Int] = MyIO(() =>
    println("Calculating...")
    42
  )

  val currentTimeIO: MyIO[Long] = MyIO( () => System.currentTimeMillis() )

  def measure[A](ioa: MyIO[A]): MyIO[Long] = for {
    start <- currentTimeIO
    _     <- ioa
    end   <- currentTimeIO
  } yield end - start

  def testMeasureIO(): Unit = {
    val test = measure(MyIO(() => Thread.sleep(1000)))
    println(test.unsafeRun())
  }
  def putStrLn(s: String): MyIO[Unit] = MyIO(() => println(s))
  def getStrLn: MyIO[String] = MyIO( () => scala.io.StdIn.readLine())

  val prog: MyIO[Unit] = for {
    _ <- putStrLn("What is your name?")
    name <- getStrLn
    _ <- putStrLn(s"Hello $name")
  } yield ()

  override def run: IO[Unit] =
    IO.println("Effects") *>
    IO.println("1------") *>
    IO.println(io.unsafeRun()) *>
    IO.println("2------") *>
    IO.println(testMeasureIO()) *>
    IO.println("3------") *>
    IO(prog.unsafeRun()) *>
    IO.println("4------") *>
    IO.unit
}
