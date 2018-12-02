package com.nullhappens.cats.cases.async

import cats.Id

class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
  override def getUptime(hostname: String): Int =
    hosts.getOrElse(hostname, 0)
}

