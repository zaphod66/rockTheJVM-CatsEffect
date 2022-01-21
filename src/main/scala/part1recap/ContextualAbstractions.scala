package part1recap

import cats.effect.{IO, IOApp}

object ContextualAbstractions extends IOApp.Simple {

  // given.using combo
  def incr(x: Int)(using amount: Int): Int = x + amount
  given defaultAmount: Int = 10
  val twelve: Int = incr(2)  // (10) given by the compiler

  def mult(x: Int)(using factor: Int): Int = x * factor
  val twenty: Int = mult(2)

  // more complex example

  trait MyMonoid[A] {
    def combine(x: A, y: A): A
    def empty: A
  }

  def combineAll[A](vals: List[A])(using monoid: MyMonoid[A]): A =
    vals.foldLeft(monoid.empty)(monoid.combine)

  given intMonoid: MyMonoid[Int] with {
    override def combine(x: Int, y: Int): Int = x + y
    override def empty: Int = 0
  }

  val numbers: List[Int] = (1 to 10).toList
  val sum10: Int = combineAll(numbers)

  given stringMonoid: MyMonoid[String] with {
    override def combine(x: String, y: String): String = x + y
    override def empty: String = ""
  }

  val strings: String = combineAll(List("a", "bb", "ccc"))

  // synthesize given instances
  given optionMonoid[T](using monoid: MyMonoid[T]): MyMonoid[Option[T]] with {
    override def combine(xM: Option[T], yM: Option[T]): Option[T] = for {
      x <- xM
      y <- yM
    } yield monoid.combine(x, y)

    override def empty: Option[T] = Option(monoid.empty)
  }

  val sum10M: Option[Int] = combineAll(numbers.map(x => Option(x)))

  // extension methods

  case class Person(name: String) {
    def greet(): String = s"Hi, my name is $name"
  }

  extension (name: String)
    def greet(): String = Person(name).greet()

  val alicesGreeting: String = "Alice".greet()

  // generic extensions
  extension [T](list: List[T])
    def reduceAll(using monoid: MyMonoid[T]): T =
      list.foldLeft(monoid.empty)(monoid.combine)

  val sum10_v2: Int = numbers.reduceAll

  override def run: IO[Unit] =
    IO.println("ContextualAbstractions")
}

// type class pattern in Scala3

object TypeClasses extends IOApp.Simple {
  case class Person(name: String, age: Int)
  // part 1 - Type Class Definition
  trait JSONSer[T] {
    def toJson(value: T): String
  }

  // part 2 - define Type Class instances
  given stringSer: JSONSer[String] with {
    override def toJson(value: String): String = "\"" + value + "\""
  }

  given intSer: JSONSer[Int] with {
    override def toJson(value: Int): String = value.toString
  }

  given personSer: JSONSer[Person] with {
    override def toJson(person: Person): String =
      s"""
         |{"name": "${person.name}", "age": ${person.age}}
         |""".stripMargin.trim
  }

  // part 3 - user-facing API
  def convert2Json[T](value: T)(using ser: JSONSer[T]): String = ser.toJson(value)

  def convertList2Json[T](list: List[T])(using ser: JSONSer[T]): String =
    list.map(v => ser.toJson(v)).mkString("[", ",", "]")

  // part 4 - extension methods
  extension [T](value: T)
    def toJson(using ser: JSONSer[T]): String = ser.toJson(value)

  extension [T](list: List[T])
    def toJson(using ser: JSONSer[T]): String = convertList2Json(list)

  val persons = List(Person("Alice", 42), Person("Bob", 23))

  override def run: IO[Unit] =
    IO.println("type classes") *>
      IO.println(Person("Clara", 34).toJson) *>
      IO.println(convertList2Json(persons)) *>
      IO.println(persons.toJson)
}
