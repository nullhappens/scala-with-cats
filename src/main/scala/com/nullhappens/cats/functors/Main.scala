package com.nullhappens.cats.functors

import cats.Functor
import cats.syntax.functor._

object Main extends App {

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  object Tree {

    implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
      override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
        fa match {
          case Branch(left, right) => Branch(map(left)(f), map(right)(f))
          case Leaf(value) => Leaf(f(value))
        }
    }

    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

    def leaf[A](value: A): Tree[A] = Leaf(value)
  }

  val tree = Tree.branch(Tree.leaf(10), Tree.leaf(22))
  val modifiedTree = Functor[Tree].map(tree)(_ * 2)
  val modifiedTree2 = tree.map(_ * 2)
  assert(modifiedTree == modifiedTree2)
  println(tree)
  println(modifiedTree)

}
