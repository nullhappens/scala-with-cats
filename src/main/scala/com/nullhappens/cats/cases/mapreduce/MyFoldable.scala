package com.nullhappens.cats.cases.mapreduce

import cats.Monoid


object MyFoldable {

  def foldMap[A, B : Monoid](seq: Vector[A])(func: A => B): B =
    seq.map(func).foldLeft(Monoid.empty[B])(Monoid.combine[B])
  // in one step:
  // as.foldLeft(Monoid[B].empty)(_ |+| func(_))
}

object MyFoldableTests extends App {
  import cats.instances.int._
  val r1: Int = MyFoldable.foldMap(Vector(1, 2, 3))(identity)
  assert(r1 == 6)

  import cats.instances.string._
  val r2: String = MyFoldable.foldMap(Vector(1, 2, 3))(_.toString + "! ")
  assert("1! 2! 3! " == r2)

  val r3: String = MyFoldable.foldMap("Hello world!".toVector)(_.toString.toUpperCase())
  assert(r3 == "HELLO WORLD!")
}
