package com.nullhappens.cats.monads

import cats.Eval

object EvalMonadExcercises extends App {
  val now = Eval.now{
    println("computing now!")
    math.random() + 1000
  }
  println(now)
  println(now.value)
  println(now.value)

  val later = Eval.later{
    println("computing later!")
    math.random() + 1000
  }
  println(later)
  println(later.value)
  println(later.value)

  val always = Eval.always{
    println("computing always")
    math.random() + 1000
  }
  println(always)
  println(always.value)
  println(always.value)

  val greeting = Eval.always{
    println("Step 1")
    "Hello"
  }.map{ str =>
    println("Step 2")
    s"$str World!"
  }
  println(greeting.value)

  val ans = for {
    a <- Eval.now{ println("Calculating A"); 40 }
    b <- Eval.always{ println("Calculating B"); 2}
  } yield {
    println("Adding A and B")
    a + b
  }
  println(ans)
  println(ans.value)
  println(ans.value)

  val saying = Eval.always{
    println("Step 1")
    "The Cat"
  }.map { str =>
    println("Step 2")
    s"$str sat on"
  }.memoize
    .map{ str =>
      println("Step 3")
      s"$str the mat"
    }
  println(saying)
  println(saying.value)
  println(saying.value)

  // Stack safe factorial

  // Basic recursive implementation overflows
  //  def factorial(n: BigInt): BigInt =
  //    if (n == 1) n
  //    else n * factorial(n - 1)

  //This one still overflows
  //  def factorial(n: BigInt): Eval[BigInt] =
  //    if (n == 1)
  //      Eval.now(n)
  //    else
  //      factorial(n - 1).map(_ * n)

  // Using Eval.defer to trampoline
  def factorial(n: BigInt): Eval[BigInt] =
    if (n == 1)
      Eval.now(n)
    else
      Eval.defer(factorial(n - 1).map(_ * n))

  println(factorial(50000).value)

  // Naive non stack safe foldRight
//  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
//    as match {
//      case head :: tail =>
//        fn(head, foldRight(tail, acc)(fn))
//      case Nil =>
//        acc
//    }

  // Rewritten to be stack safe below:
  def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      case Nil =>
        acc
    }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldRightEval(as, Eval.now(acc)){ (a, b) =>
      b.map(fn(a, _))
    }.value

  println(foldRight((1 to 1000000).toList, 0L)(_ + _))

}
