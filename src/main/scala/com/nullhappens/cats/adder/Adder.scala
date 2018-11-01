package com.nullhappens.cats.adder

import cats.Monoid
import cats.instances.int._
import cats.instances.option._
import cats.syntax.semigroup._

object Adder {
  // cleanest syntax using "context bound syntax"
  def add[A: Monoid](items: List[A]): A =
    items.fold(Monoid[A].empty)(Monoid[A].combine)

  // equivalent to add but using implicits
  def add2[A](items: List[A])(implicit m: Monoid[A]): A =
    items.fold(m.empty)(m.combine)

  // equivalent to add and add2 but using semigroup syntax
  def add3[A](items: List[A])(implicit m: Monoid[A]): A =
    items.fold(m.empty)(_ |+| _)
}

final case class Order(totalCost: Double, quantity: Double)
object Order {
  implicit val orderSum: Monoid[Order] =
    new Monoid[Order] {
      def empty: Order                       = Order(0, 0)
      def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
    }
}

object AdderMain extends App {
  val lst = List(1, 2, 3, 4, 5, 6, 7, 78, 8, 9, 78675, 4565)
  assert(Adder.add(lst) == 83363)
  assert(Adder.add2(lst) == 83363)
  assert(Adder.add3(lst) == 83363)

  assert(Adder.add(List(Some(1), Some(2), None)).contains(3))
  assert(Adder.add2(List(Some(1), Some(2), None)).contains(3))
  assert(Adder.add3(List(Some(1), Some(2), None)).contains(3))

  assert(Adder.add(List(Option(1), Option(2))).contains(3))

  val orderA     = Order(20, 10)
  val orderB     = Order(10, 5)
  val totalOrder = Order(30, 15)
  assert(Adder.add(List(orderA, orderB)) == totalOrder)
}
