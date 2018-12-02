package com.nullhappens.cats.validated

import cats.Semigroupal
import cats.data.Validated
import cats.instances.list._

object ValidatedExcercises extends App{

  // Validated has an instance of Semigroupal but no instance of Monad (no short circuiting)
  type AllErrorsOr[A] = Validated[List[String], A]

  val res1 = Semigroupal[AllErrorsOr].product(
    Validated.invalid(List("Error 1")),
    Validated.invalid(List("Error 2"))
  )
  println(res1)

  // Syntax options
  // #1
  val r1 = Validated.Valid(123)
  val i1 = Validated.valid[List[String], Int](123)
  println(r1)
  println(i1)
  // #2
  val r2 = Validated.invalid(List("Badness"))
  val i2 = Validated.invalid[List[String], Int](List("Badness"))
  println(r2)
  println(i2)
  // #3
  import cats.syntax.validated._
  val r3 = 123.valid[List[String]]
  val i3 = List("Badness").invalid[Int]
  println(r3)
  println(i3)
  // #4
  import cats.syntax.applicative._
  import cats.syntax.applicativeError._

  type ErrorsOr[A] = Validated[List[String], A]
  val r4 = 123.pure[ErrorsOr]
  val i4 = List("Badness").raiseError[ErrorsOr, Int]
  println(r4)
  println(i4)

  val res7 = Validated.catchOnly[NumberFormatException]("foo".toInt)
  println(res7)

  val res8 = Validated.catchNonFatal(sys.error("Some Badness"))
  println(res8)

  val res9 = Validated.fromTry(scala.util.Try("foo".toInt))
  println(res9)

  val res10 = Validated.fromEither[String, Int](Left("Badness"))
  println(res10)

  val res11 = Validated.fromOption[String, Int](None, "Badness")
  println(res11)

  // Validated accumulates errors using a Semigroup
  val res13: Semigroupal[AllErrorsOr] = Semigroupal[AllErrorsOr]
  println(res13)

  import cats.instances.vector._
  import cats.syntax.apply._

  val res15 = (
    Vector(404).invalid[Int],
    Vector(500).invalid[Int]
  ).tupled
  println(res15)

  import cats.data.NonEmptyVector

  val res16 = (
    NonEmptyVector.of("Error 1").invalid[Int],
    NonEmptyVector.of("Error 2").invalid[Int]
  ).tupled
  println(res16)

  println(123.valid.map(_ * 100))
  println("?".invalid.leftMap(_.toString))
  println(123.valid[String].bimap(_ + "!", _ * 100))

  // Validated has no flatMap but has andThen
  val res17: Validated[String, Int] =
    32.valid.andThen { a =>
      10.valid.map { b =>
        a + b
      }
    }
  println(res17)

  import cats.syntax.either._
  val res18: Validated[String, Int] = "Badness".invalid[Int]
  val res19: Either[String, Int] = res18.toEither
  val res20: Validated[String, Int] = res19.toValidated

  val res21: Validated[String, Int] = 123.valid[String].ensure("Negative!")(_ > 0)

  val res22: Int = "fail".invalid[Int].getOrElse(0)

  val res23: String = "fail".invalid[Int].fold(_ + "!!!", _.toString)

  // Exercise 6.4.4

  final case class User(name: String, age: Int)

  // validation rules
  // Name and age must be specified
  // Name must not be blank
  // Age must be a valid non negative integer

  type FormData = Map[String, String]
  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]

  def getValue(name: String)(data: FormData): FailFast[String] =
    data.get(name)
      .toRight(List(s"$name not specified"))

  def parseInt(name: String)(data: String): FailFast[Int] =
    Either.catchOnly[NumberFormatException](data.toInt)
      .leftMap(_ => List(s"$name must be an integer"))

  def nonEmptyString(name: String)(data: String): FailFast[String] =
    Right(data).ensure(List(s"$name cannot be blank"))(_.nonEmpty)

  def nonNegative(name: String)(data: Int): FailFast[Int] =
    Right(data).ensure(List(s"$name cannot be negative"))(_ >= 0)

  def readName(form: FormData): FailFast[String] =
    for {
      v <- getValue("name")(form)
      r <- nonEmptyString("name")(v)
    } yield r

  def readAge(form: FormData): FailFast[Int] =
    for {
      v <- getValue("age")(form)
      n <- parseInt("age")(v)
      r <- nonNegative("age")(n)
    } yield r

  def readUser(form: FormData): FailSlow[User] =
    (
      readName(form).toValidated,
      readAge(form).toValidated
    ).mapN(User.apply)


  import cats.implicits._
  assert(readName(Map("name" -> "Diego")) === Right("Diego"))
  assert(readName(Map()) === Left(List("name not specified")))
  assert(readName(Map("name" -> "")) === Left(List("name cannot be blank")))

  assert(readAge(Map("age" -> "22")) === Right(22))
  assert(readAge(Map()) === Left(List("age not specified")))
  assert(readAge(Map("age" -> "a")) === Left(List("age must be an integer")))
  assert(readAge(Map("age" -> "-22")) === Left(List("age cannot be negative")))

  val formData = Map(
    "name" -> "Diego",
    "age" -> "36"
  )
  val value: FailSlow[User] = readUser(formData)
  println(value)

  val invalidFormData = Map(
    "age" -> "a"
  )
  println(readUser(invalidFormData))
}
