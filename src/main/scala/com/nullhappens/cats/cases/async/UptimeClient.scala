package com.nullhappens.cats.cases.async

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}
