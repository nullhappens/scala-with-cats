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



}
