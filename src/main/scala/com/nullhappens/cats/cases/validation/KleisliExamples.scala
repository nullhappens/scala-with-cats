package com.nullhappens.cats.cases.validation

import cats.data.{Kleisli, NonEmptyList}
import cats.instances.list._

object KleisliExamples extends App {

  // Transform an input Int into an output of type List[Int]

  val step1: Kleisli[List, Int, Int] = Kleisli(x => List(x + 1, x - 1))
  val step2: Kleisli[List, Int, Int] = Kleisli(x => List(x, -x))
  val step3: Kleisli[List, Int, Int] = Kleisli(x => List(x * 2, x / 2))
  val pipeline: Kleisli[List, Int, Int] = step1 andThen step2 andThen step3

  println(pipeline.run(20))

}

object KleisliValidation extends App {
  type Errors = NonEmptyList[String]

  type Result[A] = Either[Errors, A]

  type Check[A, B] = Kleisli[Result, A, B]

  def error(s: String): NonEmptyList[String] = NonEmptyList(s, Nil)

  def check[A, B](func: A => Result[B]): Check[A, B] = Kleisli(func)

  def checkPred[A](pred: Predicate[Errors, A]): Check[A, A] = Kleisli[Result, A, A](pred.run)
}
