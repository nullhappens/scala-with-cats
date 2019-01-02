package com.nullhappens.cats.cases.crdt

final case class GCounter1(counters: Map[String, Int]) {

  def increment(machine: String, amount: Int): GCounter1 = {
    val newCount = counters.getOrElse(machine, 0) + amount
    GCounter1(counters + (machine -> newCount))
  }

  def merge(that: GCounter1): GCounter1 =
    GCounter1(that.counters ++ this.counters.map{
      case (k, v) => k -> (v max that.counters.getOrElse(k, 0))
    })

  def total: Int = counters.values.sum

}
