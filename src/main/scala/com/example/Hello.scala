package com.example

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.example.actors.{Auction, Buyer}

import concurrent.duration._

import scala.concurrent.ExecutionContext.Implicits.global

object HelloSimpleMain {

  val system = ActorSystem("Auction-actor-system")

  def auction(name: String, price: Int): ActorRef = {
    system.actorOf(Props(classOf[Auction], name, price), name = name.replace(" ", "-"))
  }

  def buyer(name: String, duration: FiniteDuration, auctions: List[ActorRef]): ActorRef = {
    system.actorOf(Props(classOf[Buyer], name, duration, auctions), name = name.replace(" ", "-"))
  }

  def main(args: Array[String]): Unit = {

    val tshirtAuctions = List(auction("t-shirt xl", 10), auction("t-shirt red", 5), auction("shirt big", 100))

    buyer("tom", 2 seconds, tshirtAuctions)
    buyer("bob", 6 seconds, tshirtAuctions)
    buyer("some guy", 10 seconds, tshirtAuctions)


    val computerAuctions = List(auction("macbook pro", 1000), auction("cheap old computer", 100))
    
    buyer("computer-guy", 20 seconds, computerAuctions)
    buyer("hackerman", 43 seconds, computerAuctions)

  }

}
