package com.nullhappens.cats.cases.validation

import cats.data.NonEmptyList

object ValidationExamples extends App{

  type Errors = NonEmptyList[String]

  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)

  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be longer than $n characters"),
      str => str.length > n
    )

  def alphanumeric: Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be all alphanumeric characters"),
      str => str.forall(_.isLetterOrDigit)
    )

  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char only once"),
      str => str.count(c => c == char) == 1
    )


  // Username must contain at least four characters and consists entirely of
  // alphanumeric characters
  def checkUsername(userName: String): Check[Errors, String, String] =
    Check(longerThan(3) and alphanumeric)

  // An email address must contain an @ sign.  Split the string at the @.  The
  // string to the left must not be empty.  The string to the right must be at least
  // three characters long and contain a dot.


  //  NONE OF THIS CODE COMPILES, RECHECK LATER
  //  val splitEmail: Check[Errors, String, (String, String)] =
  //    Check(a:String => a.split('@') match {
  //      case Array(name, domain) =>
  //        (name, domain).validNel[String]
  //
  //      case other =>
  //        "Must contain a single @ character".
  //          invalidNel[(String, String)]
  //    })
  //
  //  val checkLeft: Check[Errors, String, String] = Check(longerThan(0))
  //
  //  val checkRight: Check[Errors, String, String] = Check(longerThan(3) and contains('.'))
  //
  //  val joinEmail: Check[Errors, (String, String), String] =
  //    Check {
  //      case (l, r) => (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)
  //    }
  //
  //  val checkEmail: Check[Errors, String, String] = splitEmail andThen joinEmail

  //  final case class User(username: String, email: String)
  //
  //  def createUser(username: String, email: String): Validated[Errors, User] =
  //    (checkUsername(username), checkEmail(email)).mapN(User)
}
