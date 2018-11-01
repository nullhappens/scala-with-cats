package com.nullhappens.cats.show

import cats._
import cats.implicits._
import com.nullhappens.cats.printable.Cat

object Main extends App {
  implicit val catShow: Show[Cat] =
    Show.show(cat => s"${cat.name.show} is a ${cat.age.show} year-old ${cat.color.show} cat.")
  println(Cat("Mr Buttons", 5, "Brown").show)
}
