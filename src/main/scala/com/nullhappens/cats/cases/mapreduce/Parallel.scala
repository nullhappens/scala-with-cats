package com.nullhappens.cats.cases.mapreduce

import cats.{Applicative, Monoid}
import cats.syntax.monoid._
import cats.syntax.traverse._
import cats.syntax.foldable._
import cats.syntax.functor._

import cats.instances.vector._
import cats.instances.future._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object Parallel {

  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val numCores: Int = Runtime.getRuntime.availableProcessors
    val groupSize: Int = (1.0 * values.size / numCores).ceil.toInt
    val groups: Iterator[Vector[A]] = values.grouped(groupSize)
    val futures: Iterator[Future[B]] = groups.map(group => Future(MyFoldable.foldMap(group)(func)))
    Future.sequence(futures).map { xs =>
      xs.foldLeft(Monoid[B].empty)(_ |+| _)
    }
  }

  // Using all cats
  def parallelFoldMap2[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val numCores: Int = Runtime.getRuntime.availableProcessors
    val groupSize: Int = (1.0 * values.size / numCores).ceil.toInt
    values
      .grouped(groupSize)
      .toVector
      .traverse(group => Future(group.foldMap(func)))
      .map(_.combineAll)
  }

  // Generic
  def parallelFoldMap3[F[_]: Applicative, A, B: Monoid](values: Vector[A])(func: A => B): F[B] = {
    val numCores: Int = Runtime.getRuntime.availableProcessors
    val groupSize: Int = (1.0 * values.size / numCores).ceil.toInt
    values
      .grouped(groupSize)
      .toVector
      .traverse(group => Applicative[F].pure(group.foldMap(func)))
      .map(_.combineAll)
  }

}

object Main extends App {
  import cats.instances.int._
  import scala.concurrent.duration._

  val result: Future[Int] = Parallel.parallelFoldMap((1 to 1000000).toVector)(identity)
  println(Await.result(result, 1.second))

  val result2: Future[Int] = Parallel.parallelFoldMap2((1 to 1000000).toVector)(_ * 1000)
  println(Await.result(result2, 1.second))

  val result3: Future[Int] = Parallel.parallelFoldMap3[Future, Int, Int]((1 to 1000000).toVector)(_ * 1000)
  println(Await.result(result3, 1.second))

}
