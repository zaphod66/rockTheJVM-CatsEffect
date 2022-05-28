package FreeMonad

import cats.{Monad, ~>}
import cats.effect.{IO, IOApp}
import utils.debug

import scala.collection.mutable

// https://blog.rockthejvm.com/free-monad/

object FreeMonad extends IOApp.Simple {

  trait Free[M[_], A] {
    import Free.*

    def flatMap[B](f: A => Free[M, B]): Free[M, B] = FlatMap(this, f)
    def map[B](f: A => B): Free[M, B] = flatMap(a => pure(f(a)))
    def foldMap[G[_]: Monad](trans: M ~> G): G[A] = this match
      case Pure(a)        => Monad[G].pure(a)
      case FlatMap(fa, f) => Monad[G].flatMap(fa.foldMap(trans))(a => f(a).foldMap(trans))
      case Suspend(ma)    => trans(ma)
  }

  object Free {
    def pure[M[_], A](a: A): Free[M, A] = Pure(a)
    def liftM[M[_], A](ma: M[A]): Free[M, A] = Suspend(ma)

    case class Pure[M[_], A](a: A) extends Free[M, A]
    case class FlatMap[M[_], A, B](fa: Free[M, A], f: A => Free[M, B]) extends Free[M, B]
    case class Suspend[M[_], A](ma: M[A]) extends Free[M, A]
  }

  // Motivation

  // 1. Algebra
  trait DBOps[A]
  case class Create[A](key: String, value: A) extends DBOps[Unit]
  case class Read[A](key: String) extends DBOps[A]
  case class Update[A](key: String, value: A) extends DBOps[A]
  case class Delete(key: String) extends DBOps[Unit]

  // definition
  type DBMonad[A] = Free[DBOps, A]

  // 2. "smart" constructors
  def create[A](key: String, value: A): DBMonad[Unit] = Free.liftM[DBOps, Unit](Create(key, value))
  def read[A](key: String): DBMonad[A]                = Free.liftM[DBOps, A](Read(key))
  def update[A](key: String, value: A): DBMonad[A]    = Free.liftM[DBOps, A](Update(key, value))
  def delete(key: String): DBMonad[Unit]              = Free.liftM[DBOps, Unit](Delete(key))

  // 3. business logic
  def program: DBMonad[Unit] = for {
    _     <- create[String]("123", "Mona")
    name1 <- read[String]("123")
    _     <- create[String]("456", name1.toUpperCase)
    _     <- delete("123")
    name2 <- read[String]("456")
    _     <- update[String]("456", name2.reverse)
  } yield ()

  // 4. evaluate the program (transform into a real effect)

  val memDB: mutable.Map[String, String] = mutable.Map[String, String]()

  // ToDo: use real serializers
  def ser[A](a: A): String = a.toString
  def des[A](s: String): A = s.asInstanceOf[A]

  // Natural transformation DBOps ~> IO
  val dbOps2IO: DBOps ~> IO = new (DBOps ~> IO) {
    override def apply[A](fa: DBOps[A]): IO[A] = fa match
      case Create(k, v) =>
        for {
          _ <- IO(s"INSERT INTO people(id, name) VALUES ($k, $v);").debug
          _ = memDB += (k -> ser(v))
        } yield ()
      case Read(k) =>
        for {
          _ <- IO(s"SELECT * FROM people WHERE id=$k LIMIT 1;").debug
          r = des[A](memDB(k))
        } yield r
      case Update(k, v) =>
        for {
          _ <- IO(s"UPDATE people(name=$v) WHERE id=$k;").debug
          o = memDB(k)
          _ = memDB += (k -> ser(v))
          r = des[A](o)
        } yield r
      case Delete(k) =>
        for {
          _ <- IO(s"DELETE FROM people WHERE id=$k;").debug
          _ = memDB.remove(k)
        } yield ()
  }

  val ioProgram: IO[Unit] = program.foldMap(dbOps2IO)

  override def run: IO[Unit] =
    IO("FreeMonad").debug *>
    IO("1--------").debug *>
    IO(program.toString).debug *>
    IO("2--------").debug *>
    ioProgram *>
    IO("3--------").debug *>
    IO(memDB.toString()).debug *>
    IO("4--------").debug *>
    IO.unit

}
