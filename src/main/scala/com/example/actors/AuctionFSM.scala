package com.example.actors

import akka.actor.{ActorLogging, ActorRef, FSM}

import scala.concurrent.duration._

import scala.concurrent.ExecutionContext.Implicits.global

sealed trait State
case object Created extends State
case object Ignored extends State
case object Activated extends State
case object Sold extends State


sealed trait Data
case object Uninitalized extends Data
final case class HighestBid(cents: Int, byWho: ActorRef) extends Data

sealed trait AuctionMessage
case class Bid(cents: Int, bidder: ActorRef) extends AuctionMessage
case object Relist extends AuctionMessage
case object BidTimerExpired extends AuctionMessage
case object DeleteTimerExpired extends AuctionMessage


// TODO: - auction become
// TODO: - timers in fsm
class Auction(val auctionName: String, initialPrice: Int = 0) extends FSM[State, Data] with ActorLogging {

  log.info(s"Created auction $auctionName with initialPrice $initialPrice")

  private val BIDDING_TIMER_DURATION = 30 seconds
  private val DELETE_TIMER_DURATION = 10 seconds

  startWith(Created, Uninitalized)
  startBidTimer()

  when(Created) {
    case Event(Bid(cents, buyerRef), Uninitalized) => {

      if(cents > initialPrice) {
        log.info(s"${buyerRef.path.name} bid $cents.")
        goto(Activated) using HighestBid(cents, buyerRef)
      }
      else {
        log.info(s"${buyerRef.path.name} bid $cents but it is lower than initial price.")
        stay using Uninitalized
      }
    }
    case Event(BidTimerExpired, state @ _) => {
      log.info("Auction expired and will be ignored")
      startDeleteTimer()
      goto(Ignored) using state
    }
  }

  when(Ignored) {
    case Event(Relist, _) => {
      goto(Created) using Uninitalized
    }
    case Event(DeleteTimerExpired, _) => {
      log.info("Delete timer expired, actor was stopped.")
      stop()
    }
    case Event(Bid(_, _), _) => {
      log.info("Someone is bidding in ignored state, ignoring")
      stay()
    }
  }

  when(Activated) {
    case Event(Bid(newBidCents, newBuyerRef), oldBid @ HighestBid(oldCents, oldBuyer)) => {
      if (newBidCents > oldCents) {
        log.info(s"${newBuyerRef.path.name} bid $newBidCents and this is higher than old bid: $oldCents by ${oldBuyer.path.name}")
        stay using HighestBid(newBidCents, newBuyerRef)
      }
      else {
        log.info(s"${newBuyerRef.path.name} bid $newBidCents but this is too low to beat $oldCents by ${oldBuyer.path.name}")
        stay using oldBid
      }
    }
    case Event(BidTimerExpired, state @ HighestBid(price, buyer)) => {
      log.info(s"Auction was sold, result for $price to ${buyer.path.name}")
      startDeleteTimer()
      notifyBuyer(buyer, price)
      goto(Sold) using state
    }
  }

  when(Sold) {
    case Event(DeleteTimerExpired, state @ HighestBid(price, _)) => {
      log.info(s"Auction was deleted after being sold, result price: $price")
      stop()
    }
    case Event(Bid(_, _), _) => {
      log.info("Someone is bidding after auction was sold, ignoring")
      stay()
    }
  }

  def notifyBuyer(buyer: ActorRef, price: Int): Unit = {
    buyer ! Won(auctionName, price)
  }

  def startBidTimer(): Unit = {
    context.system.scheduler.scheduleOnce(BIDDING_TIMER_DURATION, self, BidTimerExpired)
  }

  def startDeleteTimer(): Unit = {
    context.system.scheduler.scheduleOnce(DELETE_TIMER_DURATION, self, DeleteTimerExpired)
  }

}

object Auction {
  def apply(auctionName: String, initialPrice: Int = 0): Auction = new Auction(auctionName, initialPrice)
}
