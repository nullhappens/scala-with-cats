package com.nullhappens.cats.monads

import cats.data.Writer
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object WriterMonadExercises extends App {
  val w1 = Writer(Vector("It was the best of times", "It was the worst of times"), 1859)
  println(w1)

  // When we have only a result (with no log)
  type Logged[A] = Writer[Vector[String], A]
  val w2 = 123.pure[Logged]
  println(w2)

  // When we have a log but no result
  val w3 = Vector("msg1", "ms2", "msg3").tell
  println(w3)

  // Otherwise we can use either Writer.apply or writer syntax
  val w4 = Writer(Vector("msg1", "msg2", "msg3"), 123)
  println(w4)

  val w5 = 123.writer(Vector("msg1", "msg2", "msg3"))
  println(w5)
  println(w5.value)
  println(w5.written)

  val (log, result) = w5.run
  println(log)
  println(result)

  // Composing and transforming writers
  val writer1 = for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield a + b
  println(writer1.run)

  // transforming the log only
  val writer2 = writer1.mapWritten(_.map(_.toUpperCase()))
  println(writer2.run)

  // transforming both log and value
  val writer3 = writer1.bimap(
    _.map(_.toUpperCase),
    _ * 100
  )
  println(writer3.run)

  val writer4 = writer1.mapBoth{ (log, res ) =>
    (log.map(_ + "!"), res * 1000)
  }
  println(writer4.run)

  // clearing the log and swapping
  val writer5 = writer1.reset
  println(writer5.run)

  val writer6 = writer1.swap
  println(writer6.run)

  // Exercise 4.7.3
  def slowly[A](body: => A) =
    try body finally Thread.sleep(500)

  def factorial(n: Int): Int = {
    val ans = slowly(if(n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }
  println(factorial(5))

  // This code logs different results from different computations
  Await.result(Future.sequence(Vector(
    Future(factorial(3)),
    Future(factorial(3))
  )), 10.seconds)

  // Rewriting factorial to use Writer
  def factorial2(n: Int): Logged[Int] =
    for {
      ans <- if (n == 0) 1.pure[Logged]
      else slowly(factorial2(n - 1).map(_ * n))
      _ <- Vector(s"fact $n $ans").tell
    } yield ans

  val results =
    Await.result(Future.sequence(Vector(
      Future(factorial2(3).run),
      Future(factorial2(3).run)
    )), 10.seconds)
  for ( (log, _) <- results; line <- log) println(line)


}
