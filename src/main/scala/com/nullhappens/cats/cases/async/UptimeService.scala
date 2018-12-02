package com.nullhappens.cats.cases.async

import cats.Applicative
import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.traverse._

class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {

  def getTotalUptime(hostnames: List[String]): F[Int] =
     hostnames.traverse(client.getUptime).map(_.sum)

}
