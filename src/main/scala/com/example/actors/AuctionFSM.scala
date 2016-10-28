package com.example.actors

import akka.actor.{ActorLogging, ActorRef, FSM}
import com.example.actors.MessagesAndStates._

import scala.concurrent.duration._



class AuctionFSM(val auctionName: String, initialPrice: Int = 0) extends FSM[State, Data] with ActorLogging {

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
    setTimer("bidTimer", BidTimerExpired, BIDDING_TIMER_DURATION)
  }

  def cancelBidTimer(): Unit = {
    cancelTimer("bidTimer")
  }

  def startDeleteTimer(): Unit = {
    setTimer("deleteTimer", DeleteTimerExpired, DELETE_TIMER_DURATION)
  }

  def cancelDeleteTimer(): Unit = {
    cancelTimer("deleteTimer")
  }

  onTransition {
    case Activated -> Sold => startDeleteTimer()
    case Created -> Ignored => startDeleteTimer()
    case Ignored -> Activated =>
      cancelDeleteTimer()
      startBidTimer()
  }

}

