package com.nullhappens.cats.monads

import cats.data.State
import State._
import cats.syntax.applicative._

object StateMonadExcercises extends App{

  val a = State[Int, String]{ state =>
    (state, s"The state is $state")
  }

  val (state, result) = a.run(10).value
  println(state)
  println(result)

  val state2 = a.runS(10).value
  println(state2)

  val result2 = a.runA(10).value
  println(result2)

  val step1 = State[Int, String]{ num =>
    val ans = num + 1
    (ans, s"Result of step1 $ans")
  }

  val step2 = State[Int, String]{ num =>
    val ans = num * 2
    (ans, s"Result of step2 $ans")
  }

  val both = for {
    a <- step1
    b <- step2
  } yield (a, b)

  val (st, res) = both.run(20).value
  println (st)
  println(res)

  val getDemo = State.get[Int]
  println(getDemo.run(9).value)

  val setDemo = State.set[Int](22)
  println(setDemo.run(2).value)

  val pureDemo = State.pure[Int, String]("Some result")
  println(pureDemo.run(9).value)

  val inspectDemo = State.inspect[Int, String]{ num => s"The state is $num" }
  println(inspectDemo.run(99).value)

  val modifyDemo = State.modify[Int](_ + 10)
  println(modifyDemo.run(20).value)

  // putting them all together
  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 1)
    b <- get[Int]
    _ <- modify[Int](_ + 1)
    c <- inspect[Int, Int](_ * 1000)
  } yield (a, b, c)

  val (state3, result3) = program.run(1).value
  println(state3)
  println(result3)

  // Postorder calculator
  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }

  def operand(i: Int): CalcState[Int] = State[List[Int], Int]{ stack =>
    (i :: stack, i)
  }

  def operator(function: (Int, Int) => Int): CalcState[Int] = State[List[Int], Int] {
    case x1 :: x2 :: tail =>
      val ans = function(x2, x1)
      (ans :: tail, ans)
    case _ =>
      sys.error("fail!")
  }

  val program2 = for {
    _ <- evalOne("2")
    _ <- evalOne("1")
    ans <- evalOne("-")
  } yield ans


  println(s"program2: ${program2.run(Nil).value}")

  // this one would fail since stack is empty
//  val fail = for {
//    ans <- evalOne("-")
//  } yield ans
//
//  println(fail.run(Nil).value)

  // this one would fail since not evaluating numbers
//  val fail2 = for {
//    _ <- evalOne("a")
//  } yield ()
//
//  println(fail2.run(Nil).value)

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState])( (s, t) => s.flatMap(_ => evalOne(t)) )

  val program3 = evalAll(List("2", "1", "-"))
  println(program3.runA(Nil).value)

  val program4 = evalAll(List("1", "2", "+", "3", "*"))
  println(program4.runA(Nil).value)

  val program5 = for {
    _ <- evalAll(List("1", "2", "+"))
    _ <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")
  } yield ans
  println(program5.run(Nil).value)


}
