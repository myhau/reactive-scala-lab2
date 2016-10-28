package com.example.actors

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable}
import com.example.actors.MessagesAndStates._

import scala.concurrent.duration._

import scala.concurrent.ExecutionContext.Implicits.global

class Auction(val auctionName: String, initialPrice: Int = 0) extends Actor with ActorLogging {

  log.info(s"Created auction $auctionName with initialPrice $initialPrice")

  private val BIDDING_TIMER_DURATION = 30 seconds
  private val DELETE_TIMER_DURATION = 10 seconds

  var ongoingDeleteTimer: Option[Cancellable] = Option.empty
  var ongoingBidTimer: Option[Cancellable] = Option.empty

  startBidTimer()

  override def receive: Receive = created

  def created: Receive = {
    case Bid(cents, buyerRef) => {
      if(cents > initialPrice) {
        log.info(s"${buyerRef.path.name} bid $cents.")
        context.become(activated(HighestBid(cents, buyerRef)))
      } else {
        log.info(s"${buyerRef.path.name} bid $cents but it is lower than initial price.")
      }
    }
    case BidTimerExpired => {
      log.info("Auction expired and will be ignored")
      startDeleteTimer()
      context.become(ignored)
    }
  }

  def ignored: Receive = {
    case Relist => {
      cancelDeleteTimer()
      startBidTimer()
      context.become(created)
    }
    case DeleteTimerExpired => {
      log.info("Delete timer expired, actor was stopped.")
      context.stop(self)
    }
    case Bid(cents, buyer) => {
      log.info("Someone is bidding in ignored state, ignoring")
    }
  }

  def activated(state: HighestBid): Receive = {
    case Bid(newBidCents, newBuyerRef) => {
      val HighestBid(oldCents, oldBuyerRef) = state
      if(newBidCents > oldCents) {
        log.info(s"${newBuyerRef.path.name} bid $newBidCents and this is higher than old bid: $oldCents by ${oldBuyerRef.path.name}")
        context.become(activated(HighestBid(newBidCents, newBuyerRef)))
      } else {
        log.info(s"${newBuyerRef.path.name} bid $newBidCents but this is too low to beat $oldCents by ${oldBuyerRef.path.name}")
      }
    }
    case BidTimerExpired => {
      val HighestBid(price, buyer) = state
      log.info(s"Auction was sold, result for $price to ${buyer.path.name}")
      notifyBuyer(buyer, price)
      startDeleteTimer()
      context.become(sold(state))
    }
  }

  def sold(state: HighestBid): Receive = {
    case Bid(_, _) => {
      log.info("Someone is bidding after auction was sold, ignoring")
    }
    case DeleteTimerExpired => {
      log.info(s"Auction was deleted after being sold, result price: ${state.cents}")
      context.stop(self)
    }
  }

  private def notifyBuyer(buyer: ActorRef, price: Int): Unit = {
    buyer ! Won(auctionName, price)
  }

  private def startBidTimer(): Unit = {
    ongoingBidTimer = Some(context.system.scheduler.schedule(BIDDING_TIMER_DURATION, BIDDING_TIMER_DURATION, self, BidTimerExpired))
  }

  private def startDeleteTimer(): Unit = {
    ongoingDeleteTimer = Some(context.system.scheduler.schedule(DELETE_TIMER_DURATION, DELETE_TIMER_DURATION, self, DeleteTimerExpired))
  }

  private def cancelBidTimer(): Unit = {
    ongoingBidTimer.exists(_.cancel())
  }

  private def cancelDeleteTimer(): Unit = {
    ongoingDeleteTimer.exists(_.cancel())
  }
}


