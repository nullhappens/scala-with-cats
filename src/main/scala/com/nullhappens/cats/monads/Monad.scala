package com.nullhappens.cats.monads

// partial definition of monad
//trait Monad[F[_]] {
//  def pure[A](a: A): F[A]
//
//  def flatMap[A, B](value: F[A])(f: A => F[B]): F[B]
//
//  def map[A, B](value: F[A])(f: A => B): F[B] =
//    flatMap(value)(a => pure(f(a)))
//}

object MonadMain extends App {
  import cats.Monad
  import cats.syntax.functor._
  import cats.syntax.flatMap._
  import scala.language.higherKinds
  import cats.instances.option._
  import cats.instances.list._

  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x * x + y * y

  println(sumSquare(Option(20), Option(10)))
  println(sumSquare(List(1, 2, 3), List(4, 5)))

  import cats.Id

  def pure[A](value: A): Id[A] = value

  def map[A, B](value: Id[A])(f: A => B): Id[B] = f(value)

  def flatMap[A, B](value: Id[A])(f: A => Id[B]): Id[B] = f(value)
}
