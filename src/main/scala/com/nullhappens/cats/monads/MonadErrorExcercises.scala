package com.nullhappens.cats.monads

import cats.MonadError
import cats.instances.either._
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.monadError._
import scala.util.Try
import cats.instances.try_._

object MonadErrorExcercises extends App{

  type ErrorOrA[A] = Either[String, A]
  val monadError = MonadError[ErrorOrA, String]

  val success: ErrorOrA[Int] = monadError.pure(45)
  println(success)

  val failure = monadError.raiseError("Some sort of error")
  println(failure)

  val handled: ErrorOrA[ErrorOrA[String]] = monadError.handleError(failure) {
    case "Some sort of error" => monadError.pure("actually this is fine")
    case other => monadError.raiseError(s"actually its not fine: $other")
  }
  println(handled)

  val ensureResult: ErrorOrA[Int] = monadError.ensure(success)("Number is too low")(_ > 1000)
  println(ensureResult)

  // using applicative error syntax

  val success2 = 45.pure[ErrorOrA]
  println(success2)
  assert(success == success2)

  val failure2 = "Some sort of error".raiseError[ErrorOrA, String]
  println(failure2)
  assert(failure == failure2)

  val ensureResult2 = success2.ensure("Number is too low")(_ > 1000)
  println(ensureResult2)
  assert(ensureResult == ensureResult2)

  val exn: Throwable = new RuntimeException("Something dun goofd")
  val triedInt: Try[Int] = exn.raiseError[Try, Int]
  println(triedInt)
}
