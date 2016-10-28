package com.example

import akka.actor.{ActorRef, ActorSystem, Props}
import com.example.actors.{Auction, AuctionFSM, Buyer}

import scala.concurrent.duration._

object Main {

  val system = ActorSystem("Auction-actor-system")

  def auctionFSM(name: String, price: Int): ActorRef = {
    system.actorOf(Props(classOf[AuctionFSM], name + "FSM", price), name = name.replace(" ", "-") + "FSM")
  }

  def auction(name: String, price: Int): ActorRef = {
    system.actorOf(Props(classOf[Auction], name + "BECOME", price), name = name.replace(" ", "-") + "BECOME")
  }

  def buyer(name: String, duration: FiniteDuration, auctions: List[ActorRef]): ActorRef = {
    system.actorOf(Props(classOf[Buyer], name, duration, auctions), name = name.replace(" ", "-"))
  }

  def main(args: Array[String]): Unit = {

    val tshirtAuctions = List(auctionFSM("t-shirt xl", 10), auctionFSM("t-shirt red", 5), auctionFSM("shirt big", 100))

    buyer("tom", 2 seconds, tshirtAuctions)
    buyer("bob", 6 seconds, tshirtAuctions)
    buyer("some guy", 10 seconds, tshirtAuctions)


    val computerAuctions = List(auctionFSM("macbook pro", 1000), auctionFSM("cheap old computer", 100))
    
    buyer("computer-guy", 20 seconds, computerAuctions)
    buyer("hackerman", 43 seconds, computerAuctions)


    val groceryAuctions = List(auction("apple", 10), auction("orange", 20), auction("banana", 50))

    buyer("become-buyer", 10 seconds, groceryAuctions)

  }

}
