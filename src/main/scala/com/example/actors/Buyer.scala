package com.example.actors

import akka.actor.{Actor, ActorLogging, ActorRef}
import com.example.actors.MessagesAndStates.Bid

import scala.concurrent.duration.FiniteDuration
import scala.util.Random
import scala.concurrent.ExecutionContext.Implicits.global

sealed trait BuyerEvents

case class Won(auctionName: String, priceCents: Int) extends BuyerEvents

case object DoBid extends BuyerEvents

class Buyer(val name: String, bidSpacing: FiniteDuration, auctions: List[ActorRef]) extends Actor with ActorLogging {


  def rebidTimerRestart(): Unit = {
    val d = Random.nextDouble() * bidSpacing + bidSpacing
    val randomFiniteDuration = d.asInstanceOf[FiniteDuration]
    context.system.scheduler.schedule(bidSpacing, randomFiniteDuration, self, DoBid)
  }

  var wonAlready: Boolean = false

  override def receive: Receive = {
    case Won(auctionName: String, price: Int) => {
      log.info(s"$name buyer won auction $auctionName for $price cents")
      context.stop(self)
    }
    case DoBid => {
      if(!wonAlready) {
        val randomAuction = Random.shuffle(auctions).head
        val randomBidAmount = Random.nextInt(1000) - 1
        log.info(s"$name buyer will bid $randomBidAmount to ${randomAuction.path.name} auction actor")
        randomAuction ! Bid(randomBidAmount, self)
      }
    }
  }

  rebidTimerRestart()

}

object Buyer {
  def apply(name: String, bidSpacing: FiniteDuration, auctions: List[ActorRef]): Buyer = new Buyer(name, bidSpacing, auctions)
}
