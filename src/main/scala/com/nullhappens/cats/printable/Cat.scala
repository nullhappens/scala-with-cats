package com.nullhappens.cats.printable

import com.nullhappens.cats.printable.PrintableInstances._

final case class Cat(name: String, age: Int, color: String)

object Cat {
  implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
    override def format(value: Cat): String = {
      val name = Printable.format(value.name)
      val age  = Printable.format(value.age)
      val color = Printable.format(value.color)
      s"$name is a $age year-old $color cat."
    }
  }
}