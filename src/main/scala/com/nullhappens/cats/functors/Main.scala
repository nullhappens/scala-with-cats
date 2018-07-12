package com.nullhappens.cats.functors

import cats.Functor

object Main extends App {

  def main(args: Array[String]): Unit = {
    sealed trait Tree[+A]

    final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

    final case class Leaf[A](value: A) extends Tree[A]

    object Tree {
      def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

      def leaf[A](value: A) = Leaf(value)
    }

    implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
       override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
        fa match {
          case Branch(left, right) => Branch(map(left)(f), map(right)(f))
          case Leaf(value) => Leaf(f(value))
        }
    }

    Tree.branch(Tree.leaf(10), Tree.leaf(22)).map(_ * 2)
  }
}
