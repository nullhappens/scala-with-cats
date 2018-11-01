package com.nullhappens.cats.monads

import cats.data.Reader
import cats.syntax.applicative._

object ReaderMonadExcercises extends App {

  case class Cat(name: String, favoriteFood: String)

  val catName: Reader[Cat, String] = Reader(_.name)
  println(catName)
  println(catName.run(Cat("Garfield", "Lasagna")))

  // Composing readers to inject values at the very end
  val greetKitty: Reader[Cat, String] = catName.map(name => s"Hello $name")
  println(greetKitty.run(Cat("Heathcliff", "junk food")))

  val feedKitty: Reader[Cat, String] = Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

  val greetAndFeed: Reader[Cat, String] =
    for {
      greet <- greetKitty
      feed  <- feedKitty
    } yield s"$greet, $feed"

  println(greetAndFeed(Cat("Garfield", "lasagne")))
  println(greetAndFeed(Cat("Heathcliff", "junk food")))

  // Excercise 4.8.3
  case class Db(usernames: Map[Int, String],
                passwords: Map[String, String])

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.exists( t => t._1 == username && t._2 == password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      u <- findUsername(userId)
      ok <- u.map(checkPassword(_, password)).getOrElse(false.pure[DbReader])
    } yield ok

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade" -> "zero",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  println(checkLogin(1, "zerocool").run(db))
  println(checkLogin(2, "acidburn").run(db))
}
