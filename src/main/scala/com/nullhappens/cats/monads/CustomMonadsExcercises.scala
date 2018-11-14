package com.nullhappens.cats.monads

import cats.Monad

import cats.syntax.flatMap._

object CustomMonadsExcercises extends App {

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A) = Leaf(value)

  implicit val treeMonad = new Monad[Tree] {

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
      fa match {
        case Branch(left, right) => branch(flatMap(left)(f), flatMap(right)(f))
        case Leaf(value) => f(value)
      }

    override def pure[A](x: A): Tree[A] = leaf(x)

    // TODO: understand this monstrosity
    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
      f(a) match {
        case Branch(l, r) =>
          Branch(
            flatMap(l) {
              case Left(l)  => tailRecM(l)(f)
              case Right(l) => pure(l)
            },
            flatMap(r) {
              case Left(r)  => tailRecM(r)(f)
              case Right(r) => pure(r)
            }
          )
        case Leaf(Left(value)) =>
          tailRecM(value)(f)

        case Leaf(Right(value)) =>
          Leaf(value)
      }
  }

  val value: Tree[Int] = branch(leaf(100), leaf(200)).flatMap(x => branch(leaf(x - 1), leaf(x + 1)))
  println(value)

//  for {
//    a <- branch(leaf(100), leaf(200))
//    b <- branch(leaf(a - 10), leaf(a + 10))
//    c <- branch(leaf(b - 1), leaf(b + 1))
//  } yield c

}

