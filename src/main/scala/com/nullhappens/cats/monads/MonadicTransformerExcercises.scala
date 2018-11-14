package com.nullhappens.cats.monads

import cats.instances.list._
import cats.instances.either._
import cats.instances.future._
import cats.instances.option._

import scala.concurrent.ExecutionContext.Implicits.global
import cats.syntax.applicative._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import cats.data.{EitherT, OptionT, Writer}

object MonadicTransformerExcercises extends App {

  type ListOption[A] = OptionT[List, A]

  val result1: ListOption[Int] = OptionT(List(Option(10)))
  val result2: ListOption[Int] = 32.pure[ListOption]

  println(result1)
  println(result2)

  private val result3: ListOption[Int] = result1.flatMap { x =>
    result2.map { y => x + y
    }
  }
  println(result3)

  // Wrapping Either around Option - We need aliases to make signatures less awful

  type EitherOr[A] = Either[String, A]
  type ErrorOrOption[A] = OptionT[EitherOr, A]


  val a: ErrorOrOption[Int] = 10.pure[ErrorOrOption]
  val b: ErrorOrOption[Int] = 32.pure[ErrorOrOption]

  val c: ErrorOrOption[Int] = a.flatMap(x => b.map(y => x + y))
  println(c)

  //Future of an Either of an Option
  type FutureEither[A] = EitherT[Future, String, A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]


  val futureEitherOr: FutureEitherOption[Int] =
    for {
      a <- 10.pure[FutureEitherOption]
      b <- 32.pure[FutureEitherOption]
    } yield a + b

  println(futureEitherOr)


  // this requires the kind projector plugin
  val intOrErr: EitherT[Option, String, Int] = 123.pure[EitherT[Option, String, ?]]
  println(intOrErr)

  // unpacking instances

  val errorStack2: OptionT[EitherOr, Int] = 32.pure[ErrorOrOption]
  println(errorStack2.value) // unpacks the transformer and gives us the inner mopnadic stack (Either[String, Option[Int]])

  val stringOrInt: Either[String, Int] = errorStack2.value.map(_.getOrElse(-1))
  println(stringOrInt)

  // calling value to unpack multiple transformers
  val intermediate = futureEitherOr.value
  val stack = intermediate.value
  val result: Either[String, Option[Int]] = Await.result(stack, 1.second)
  println(result)  // now we can unpack the Either and then the Option using regular map/fold/get/etc


  type Logged[A] = Writer[List[String], A]

  def parseNumber(str: String): Logged[Option[Int]] =
    util.Try(str.toInt).toOption match {
      case Some(x) => Writer(List(s"read $str"), Some(x))
      case None => Writer(List(s"error reading $str"), None)
    }

  // Consumers of methods use transformers internally but return monadic types
  def addAll(a: String, b: String, c:String): Logged[Option[Int]] = {
    val result = for {
      x <- OptionT(parseNumber(a))
      y <- OptionT(parseNumber(b))
      z <- OptionT(parseNumber(c))
    } yield x + y + z

    result.value
  }

  val r1 = addAll("1", "2", "3")
  println(r1)
  val r2 = addAll("1", "a", "3")
  println(r2)

  // Excercise 5.4
  type Response[A] = EitherT[Future, String, A]
  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case Some(x) => EitherT.right(Future(x))
      case None => EitherT.left(Future(s"$autobot unreachable"))
    }

  println(getPowerLevel("Jazz"))
  println(getPowerLevel("Optimus"))


  // Two autobots can perform a special move if their combined power level is 15 or greater
  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
    for {
      p1 <- getPowerLevel(ally1)
      p2 <- getPowerLevel(ally2)
    } yield (p1 + p2) > 15
  }

  println(canSpecialMove("Jazz", "Hot Rod"))
  println(canSpecialMove("Jazz", "Bumblebee"))


  def tacticalReport(ally1: String, ally2: String): String = {
    val eventualString: Future[String] = canSpecialMove(ally1, ally2).value.map {
      case Right(yes) =>
        if (yes)
          s"$ally1 and $ally2 are ready to roll out!"
        else
          s"$ally1 and $ally2 need a recharge"
      case Left(s) =>
        s"Comms error: $s"
    }
    Await.result(eventualString, 1.seconds)
  }

  println(tacticalReport("Jazz", "Bumblebee"))
  // res28: String = Jazz and Bumblebee need a recharge.

  println(tacticalReport("Bumblebee", "Hot Rod"))
  // res29: String = Bumblebee and Hot Rod are ready to roll out!

  println(tacticalReport("Jazz", "Ironhide"))
  // res30: String = Comms error: Ironhide unreachable”


}

