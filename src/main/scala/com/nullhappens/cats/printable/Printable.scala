package com.nullhappens.cats.printable

trait Printable[A] { self =>
  def format(value: A): String

  // see contravariant functors
  def contramap[B](f: B => A): Printable[B] =
    new Printable[B] {
      def format(v: B): String = self.format(f(v))
    }
}

object PrintableInstances {
  implicit val stringPrintable: Printable[String] = new Printable[String] {
    override def format(value: String): String = value
  }

  implicit val intPrintable: Printable[Int] = new Printable[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit val booleanPrintable: Printable[Boolean] = new Printable[Boolean] {
    override def format(value: Boolean): String = if (value) "yes" else "no"
  }

  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
    p.contramap[Box[A]](_.value)

}

object Printable {
  def format[A](value: A)(implicit printable: Printable[A]): String = printable.format(value)

  def print[A](value: A)(implicit printable: Printable[A]): Unit = println(format(value))
}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String = p.format(value)

    def print(implicit p: Printable[A]): Unit = println(p.format(value))
  }
}

final case class Box[A](value: A)

final case class AnotherThing(a: Int, b: Boolean)

object Main extends App {
  import com.nullhappens.cats.printable.PrintableSyntax._
  import com.nullhappens.cats.printable.PrintableInstances._

  val testCat = Cat("Mr Buttons", 5, "Brown")
  Printable.print(testCat)

  testCat.print
  assert(Printable.format(testCat) == testCat.format)

  Box("hello world").print
  Box(true).print

}
