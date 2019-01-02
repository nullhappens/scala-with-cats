package com.nullhappens.cats.cases.crdt

import cats.instances.list._
import cats.instances.map._
import cats.kernel.CommutativeMonoid
import cats.syntax.semigroup._
import cats.syntax.foldable._

final case class GeneralizedGCounter[A](counters: Map[String, A]) {

  def increment(machine: String, amount: A)(implicit m: CommutativeMonoid[A]): GeneralizedGCounter[A] = {
    val value = amount |+| counters.getOrElse(machine, m.empty)
    GeneralizedGCounter(counters + (machine -> value))
  }

  def merge(that: GeneralizedGCounter[A])(implicit b: BoundedSemiLattice[A]): GeneralizedGCounter[A] =
    GeneralizedGCounter(this.counters |+| that.counters)

  def total(implicit m: CommutativeMonoid[A]): A =
    this.counters.values.toList.combineAll

}
