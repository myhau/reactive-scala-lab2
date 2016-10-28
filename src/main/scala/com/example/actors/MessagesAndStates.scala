package com.example.actors

import akka.actor.ActorRef

object MessagesAndStates {
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
}
