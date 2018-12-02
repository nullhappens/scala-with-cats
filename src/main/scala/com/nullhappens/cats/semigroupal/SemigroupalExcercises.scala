package com.nullhappens.cats.semigroupal

import cats.Semigroupal
import cats.instances.option._
import cats.syntax.apply._

object SemigroupalExcercises extends App {

  val maybeTuple: Option[(Int, String)] = Semigroupal[Option].product(Some(123), Some("abc"))
  println(maybeTuple)

  val maybeTuple2: Option[(Nothing, String)] = Semigroupal[Option].product(None, Some("abc"))
  println(maybeTuple2)

  val maybeTuple3: Option[(Int, Int, Int)] = Semigroupal.tuple3(Option(1), Option(2), Option(3))
  println(maybeTuple3)

  val maybeTuple4: Option[(Int, Int, Int)] = Semigroupal.tuple3(Option(1), Option(2), None)
  println(maybeTuple4)

  val maybeInt: Option[Int] = Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)
  println(maybeInt)

  // Using syntax from cats.syntax.apply
  val tupled: Option[(Int, String)] = (Option(123), Option("abc")).tupled
  println(tupled)

  private val tupled2: Option[(Int, String, Boolean)] = (Option(123), Option("abc"), Option(true)).tupled
  println(tupled2)

  // mapN from syntax
  final case class Cat(name: String, born: Int, color: String)

  val maybeCat: Option[Cat] = (
    Option("Garfield"),
    Option(1978),
    Option("Orange & black")
  ).mapN(Cat.apply)
  println(maybeCat)

  import cats.Monoid
  import cats.instances.invariant._
  import cats.instances.int._
  import cats.instances.list._
  import cats.instances.string._
  import cats.syntax.apply._

  case class Cat2(name: String, yearOfBirth: Int, favoriteFoods: List[String])

  val tupleToCat: (String, Int, List[String]) => Cat2 = Cat2.apply

  val catToTuple: Cat2 => (String, Int, List[String]) = cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)

  implicit val catMonoid: Monoid[Cat2] =
    (Monoid[String], Monoid[Int], Monoid[List[String]]).imapN(tupleToCat)(catToTuple)

  import cats.syntax.semigroup._
  val garfield = Cat2("Garfield", 1978, List("Lasagne"))
  val heathcliff = Cat2("Heathcliff", 1988, List("Junk Food"))

  val weirdoCat = garfield |+| heathcliff
  println(weirdoCat)

  import cats.Semigroupal
  import cats.instances.future._
  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  val futurePair = Semigroupal[Future].product(Future("Hello"), Future(123))
  val res: (String, Int) = Await.result(futurePair, 1.second)
  println(res)

  import cats.syntax.apply._

  val futureCat: Future[Cat2] = (
    Future("Garfield"),
    Future(1978),
    Future(List("Lasagne"))
  ).mapN(Cat2.apply)

  println(Await.result(futureCat, 1.seconds))

  // Semigroupal for lists creates cartesian product
  println(Semigroupal[List].product(List(1,2), List(3, 4)))

  // Accumulating error handling
  import cats.instances.either._

  type ErrorOr[A] = Either[Vector[String], A]

  val result = Semigroupal[ErrorOr].product(
    Left(Vector("Error 1")),
    Left(Vector("Error 2"))
  )
  println(result) // Short circuit similar to flatMap

  // Exercise 6.3.11
  import cats.Monad
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  // Implement product in terms of flatMap
  def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
    for {
      a <- x
      b <- y
    } yield (a, b) // This explains why Semigroupal.product short circuits same as flatMap

}
