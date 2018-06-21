package com.nullhappens.cats.monoids

import cats.instances.string._
import cats.instances.int._
import cats.instances.option._
import cats.syntax.semigroup._

object Cats extends App {

  // Side note: using cats.Monoid to not use the monoid defined in the same package
  println(cats.Monoid[String].combine("Hello", cats.Monoid[String].combine(" ", "World")))
  println(cats.Monoid[String].empty)

  println(cats.Monoid[Int].combine(10, 22))
  println(cats.Monoid[Option[Int]].combine(Option(10), Option(22)))
  println(cats.Monoid[Option[Int]].combine(Option(10), Option.empty[Int]))
  println(cats.Monoid[Option[Int]].combine(Option(10), None))
  println(cats.Monoid[Option[Int]].combine(None, None))

  println("Well " |+| "Hello " |+| "There " |+| "Stranger" |+| cats.Monoid[String].empty)

}
