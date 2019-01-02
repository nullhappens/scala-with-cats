package com.nullhappens.cats.cases.validation

import cats.data.{Kleisli, NonEmptyList}
import cats.instances.list._
import cats.instances.either._
import cats.syntax.apply._

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

  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be longer than $n characters"),
      str => str.length > n
    )

  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be all alphanumeric characters"),
      str => str.forall(_.isLetterOrDigit)
    )

  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char"),
      str => str.contains(char)
    )

  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char only once"),
      str => str.count(c => c == char) == 1
    )

  val checkUsername: Check[String, String] = checkPred(longerThan(3) and alphanumeric)

  val splitEmail: Check[String, (String, String)] =
    check(_.split('@') match {
      case Array(name, domain) => Right((name, domain))
      case _ => Left(error("Must contain a single @ character"))
    })

  val checkLeft: Check[String, String] = checkPred(longerThan(0))

  val checkRight: Check[String, String] = checkPred(longerThan(3) and contains('.'))

  val joinEmail: Check[(String, String), String] = check {
    case (l, r) => (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)
  }

  val checkEmail: Check[String, String] = splitEmail andThen joinEmail

  final case class User(username: String, email: String)

  def createUser(userName: String, email: String): Either[Errors, User] =
    (
      checkUsername.run(userName),
      checkEmail.run(email)
    ).mapN(User)

  println(createUser("Diego", "diego@email.com"))
  println(createUser("", "diego@email.com"))
  println(createUser("Diego", ""))

}
