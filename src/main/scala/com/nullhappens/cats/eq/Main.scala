package com.nullhappens.cats.eq

import cats._
import cats.implicits._
import com.nullhappens.cats.printable.Cat

object Main extends App {
  implicit val catEq: Eq[Cat] = Eq.instance[Cat] { (c1, c2) =>
    c1.name === c2.name && c1.age === c2.age && c1.color === c2.color
  }

  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")
  val cat3 = Cat("Garfield", 38, "orange and black")

  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]
  val optionCat3 = Option(cat3)

  assert(cat1 === cat1)
  assert(cat2 === cat2)
  assert(cat1 === cat3)
  assert(cat1 =!= cat2)

  assert(optionCat1 === optionCat1)
  assert(optionCat2 === optionCat2)
  assert(optionCat1 === optionCat3)
  assert(optionCat1 =!= optionCat2)

}
