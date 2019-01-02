package com.nullhappens.cats.cases.crdt

import cats.kernel.CommutativeMonoid
import cats.syntax.semigroup._
import cats.syntax.foldable._
import cats.instances.map._
import cats.instances.list._

trait GCounter[F[_, _], K, V] {

  def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V]

  def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]

  def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V

}

object GCounter{

  def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]): GCounter[F, K, V] = counter

  implicit def mapInstance[K, V]: GCounter[Map, K, V] =
    new GCounter[Map, K, V] {
      override def increment(map: Map[K, V])(key: K, value: V)(implicit m: CommutativeMonoid[V]): Map[K, V] = {
        val total = map.getOrElse(key, m.empty) |+| value
        map + (key -> total)
      }

      override def merge(m1: Map[K, V], m2: Map[K, V])(implicit b: BoundedSemiLattice[V]): Map[K, V] =
        m1 |+| m2

      override def total(map: Map[K, V])(implicit m: CommutativeMonoid[V]): V =
        map.values.toList.combineAll
    }

  //using KeyValueStore typeclass to generalize further

  implicit def gCounterInstance[F[_, _], K, V](implicit kvs: KeyValueStore[F], km: CommutativeMonoid[F[K, V]]): GCounter[F, K, V] =
    new GCounter[F, K, V] {
      import KeyValueStore._
      override def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V] = {
        val total = kvs.getOrElse(f)(k, m.empty) |+| v
        kvs.put(f)(k, total)
      }

      override def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V] =
        f1 |+| f2

      override def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V =
        f.values.combineAll

  }

}
