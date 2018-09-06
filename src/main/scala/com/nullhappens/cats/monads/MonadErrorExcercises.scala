package com.nullhappens.cats.monads

import cats.MonadError
import cats.instances.either._
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.monadError._
import scala.util.Try
import cats.instances.try_._
import cats.Eval

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

  // Eval monad excercises

  val now = Eval.now{
    println("computing now!")
    math.random() + 1000
  }
  println(now)
  println(now.value)
  println(now.value)

  val later = Eval.later{
    println("computing later!")
    math.random() + 1000
  }
  println(later)
  println(later.value)
  println(later.value)

  val always = Eval.always{
    println("computing always")
    math.random() + 1000
  }
  println(always)
  println(always.value)
  println(always.value)

  val greeting = Eval.always{
    println("Step 1")
    "Hello"
  }.map{ str =>
    println("Step 2")
    s"$str World!"
  }
  println(greeting.value)

  val ans = for {
    a <- Eval.now{ println("Calculating A"); 40 }
    b <- Eval.always{ println("Calculating B"); 2}
  } yield {
    println("Adding A and B")
    a + b
  }
  println(ans)
  println(ans.value)
  println(ans.value)

  val saying = Eval.always{
    println("Step 1")
    "The Cat"
  }.map { str =>
    println("Step 2")
    s"$str sat on"
  }.memoize
    .map{ str =>
      println("Step 3")
      s"$str the mat"
    }
  println(saying)
  println(saying.value)
  println(saying.value)

  // Stack safe factorial

  // Basic recursive implementation overflows
  //  def factorial(n: BigInt): BigInt =
  //    if (n == 1) n
  //    else n * factorial(n - 1)

  //This one still overflows
  //  def factorial(n: BigInt): Eval[BigInt] =
  //    if (n == 1)
  //      Eval.now(n)
  //    else
  //      factorial(n - 1).map(_ * n)

  // Using Eval.defer to trampoline
  def factorial(n: BigInt): Eval[BigInt] =
    if (n == 1)
      Eval.now(n)
    else
      Eval.defer(factorial(n - 1).map(_ * n))

  println(factorial(50000).value)

}
