package com.nullhappens.cats.foldable

object FoldableExcercises extends App {

  // implement list map, flatMap, filter and sum in terms of foldRight
  // cats.Foldable gives us all of these as well
  def map[A, B](lst: List[A])(f: A => B): List[B] =
    lst.foldRight(List.empty[B])((item, accum) => f(item) :: accum)

  def flatMap[A, B](lst: List[A])(f: A => List[B]): List[B] =
    lst.foldRight(List.empty[B])((item, accum) => f(item) ++ accum)

  def filter[A](lst: List[A])(f: A => Boolean): List[A] =
    lst.foldRight(List.empty[A]){ (item, accum) =>
      if (f(item))
        item :: accum
      else
        accum
    }

  import cats.Monoid
  def sum[A](lst: List[A])(implicit m: Monoid[A]): A =
    lst.foldRight(m.empty)(m.combine)

  def sum2[A: Monoid](lst: List[A])(f: (A, A) => A): A =
    lst.foldRight(Monoid.empty[A])(f)

  import cats.instances.int._
  println(sum(List(1, 2, 3)))
  println(sum2(List(1, 2, 3))(_ + _))

  // Using foldable as a generic typeclass
  import cats.Foldable
  import cats.instances.list._

  val ints = List(1, 2, 3)
  val sum: Int = Foldable[List].foldLeft(ints, 0)(_ + _)
  println(sum)

  import cats.instances.option._

  val maybeA = Foldable[Option].foldLeft(Option(123), 10)(_ * _)
  val maybeB = Foldable[Option].foldLeft[Int, Int](None, 10)(_ * _)
  println(maybeA)
  println(maybeB)

  // Stream foldRight is not stack safe
  // def bigData = (1 to 1000000).toStream
  // println(bigData.foldRight(0L)(_ + _))

  import cats.Eval
  import cats.Foldable
  import cats.instances.stream._

  def bigData = (1 to 1000000).toStream
  val eval: Eval[Long] =
    Foldable[Stream].foldRight(bigData, Eval.now(0L)){ (num, eval) =>
      eval.map(_ + num)
    }
  println(eval.value)

  println(Foldable[Option].nonEmpty(Option(42)))
  println(Foldable[List].find(List(1, 2, 3))(_ % 2 == 0))

  // combineAll uses an implicit Monoid[Int] in scope
  val r: Int = Foldable[List].combineAll(List(1, 2, 3))
  val r1: Int = Foldable[List].fold(List(1, 2, 3))
  println(r)
  println(r1)
  assert(r == r1)

  import cats.instances.string._

  val r2: String = Foldable[List].foldMap(List(1, 2, 3))(_.toString)
  println(r2)

  import cats.instances.vector._

  val l = List(Vector(1, 2, 3), Vector(4, 5, 6))
  val r3: Int = (Foldable[List] compose Foldable[Vector]).combineAll(l)
  println(r3)

  import cats.syntax.foldable._

  println(List(1, 2, 3).combineAll)
  println(List(1, 2, 3).foldMap(_.toString))


}
