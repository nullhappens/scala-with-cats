package com.nullhappens.cats.cases.validation

import cats.Semigroup
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.instances.list._
import cats.syntax.semigroup._
import cats.syntax.either._
import cats.syntax.apply._
import cats.syntax.validated._

// Implement as a function
final case class CheckF[E, A](func: A => Either[E, A]) {

  def apply(a: A): Either[E, A] = func(a)

  def and(that: CheckF[E,A])(implicit s: Semigroup[E]): CheckF[E, A] =
    CheckF { a =>
      (this(a), that(a)) match {
        case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
        case (Left(e), Right(_)) => e.asLeft
        case (Right(_), Left(e)) => e.asLeft
        case (Right(_), Right(_)) => a.asRight
      }

    }
}

object CheckFTests extends App {

  val b: CheckF[List[String], Int] =
    CheckF { v =>
      if (v < -2) v.asRight
      else List("Must be < -2").asLeft
    }

  val a: CheckF[List[String], Int] =
    CheckF { v =>
      if (v > 2) v.asRight
      else List("Must be > 2").asLeft
    }

  val combined: CheckF[List[String], Int] = a and b

  println(combined(5))
  println(combined(0))
}

// Wrapping in object Hide so that validation.Predicate.And doesn't clash with this one
object Hide{
  // Implement as an ADT
  sealed trait CheckA[E, A] {
    def and(that: CheckA[E, A]): CheckA[E, A] = And(this, that)

    def or(that: CheckA[E, A]): CheckA[E, A] = Or(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(func) => func(a)
        case And(left, right) =>
          (left(a), right(a)).mapN((_, _) => a)
        case Or(left, right) =>
          left(a) match {
            case Valid(a) => Valid(a)
            case Invalid(e1) =>
              right(a) match {
                case Valid(a) => Valid(a)
                case Invalid(e2) => Invalid(e1 |+| e2)
              }
          }
      }
  }
  final case class And[E, A](left: CheckA[E, A], right: CheckA[E, A]) extends CheckA[E, A]
  final case class Or[E, A](left: CheckA[E, A], right: CheckA[E, A]) extends CheckA[E, A]
  final case class Pure[E, A](func: A => Validated[E, A]) extends CheckA[E, A]

  object CheckATests extends App {

    val a: CheckA[List[String], Int] =
      Pure{ v =>
        if (v > 2) v.valid
        else List("Must be > 2").invalid
      }

    val b: CheckA[List[String], Int] =
      Pure{ v =>
        if (v < -2) v.valid
        else List("Must be < -2").invalid
      }

    val combined: CheckA[List[String], Int] = a and b

    println(combined(5))
    println(combined(0))

  }
}

