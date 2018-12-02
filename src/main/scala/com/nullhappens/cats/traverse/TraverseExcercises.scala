package com.nullhappens.cats.traverse

object TraverseExcercises extends App{

  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )

  def getUptime(hostname: String): Future[Int] =
    Future(hostname.length * 60)

  // using folds
  val allUptimes: Future[List[Int]] =
    hostnames.foldLeft(Future(List.empty[Int])){ (accum, host) =>
      val uptime = getUptime(host)
      for {
        accum <- accum
        uptime <- uptime
      } yield accum :+ uptime
    }
  println(Await.result(allUptimes, 1.second))

  // This code improves using Future.traverse
  // The code for traverse is exactly the same code as above but generalized for types
  val allUptimes2: Future[List[Int]] = Future.traverse(hostnames)(getUptime)
  println(Await.result(allUptimes2, 1.second))

  // traverse can be rewritten in terms of Applicative
  import cats.Applicative
  import cats.instances.future._
  import cats.syntax.applicative._
  import cats.syntax.apply._

  def newCombine(accum: Future[List[Int]], host: String): Future[List[Int]] =
    (accum, getUptime(host)).mapN(_ :+ _)

  def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]){ (accum, item) =>
      (accum, func(item)).mapN(_ :+ _)
    }

  def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  // we can now re-implement up times above
  val totalUptime = listTraverse(hostnames)(getUptime)
  println(Await.result(totalUptime, 1.second))

  // Excersise 7.2.2.1
  import cats.instances.vector._
  val x = listSequence(List(Vector(1, 2), Vector(3, 4)))
  println(x)

  // Excercise 7.2.2.2
  import cats.instances.option._

  def process(inputs: List[Int]): Option[List[Int]] =
  listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)

  println(process(List(2, 4, 6))) // Option(List(2, 4, 6))
  println(process(List(1, 2, 3))) // None

  // Excercise 7.2.2.3
  import cats.data.Validated
  import cats.instances.list._

  type ErrorsOr[A] = Validated[List[String], A]

  def process2(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(inputs){ n =>
      if (n % 2 == 0)
        Validated.valid(n)
      else
        Validated.invalid(List(s"$n is not even"))
    }

  println(process2(List(2, 4, 6))) // Valid(List(2, 4, 6)
  println(process2(List(1, 2, 3))) // Invalid(List("1 is not even", "3 is not even")

  // Generalized cats.Traverse
  import cats.Traverse
  import cats.instances.future._
  import cats.instances.list._

  val totalUptime2: Future[List[Int]] = Traverse[List].traverse(hostnames)(getUptime)
  println(Await.result(totalUptime2, 1.second))

  val numbers = List(Future(1), Future(2), Future(3))
  val numbers2: Future[List[Int]] = Traverse[List].sequence(numbers)
  println(Await.result(numbers2, 1.second))

  // syntax versions
  import cats.syntax.traverse._

  var totalUptime3: List[Int] = Await.result(hostnames.traverse(getUptime), 1.second)
  println(totalUptime3)

  var numbers3: List[Int] = Await.result(numbers.sequence, 1.second)
  println(numbers3)
}
