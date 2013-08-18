package com.oxlade39.github.storrent.peer.protocol

import akka.actor.{ActorRef, Actor, ActorLogging, FSM}
import com.oxlade39.github.storrent._


object ClientProtocol {
  sealed trait Data
  case object Uninitialised extends Data
  case class SetPeer(peer: ActorRef)
  case class HasPeer(peer: ActorRef) extends Data

  case class Send(message: Message)

  sealed trait ChokeStatus
  sealed trait InterestStatus
  case object Choked extends ChokeStatus
  case object UnChoked extends ChokeStatus
  case object IsInterested extends InterestStatus
  case object UnInterested extends InterestStatus

  case class Status(chokeStatus: ChokeStatus, interestStatus: InterestStatus)

  case class State(client: Status, peer: Status) {
    def chokeClient = copy(client = client.copy(chokeStatus = Choked))
    def unchokeClient = copy(client = client.copy(chokeStatus = UnChoked))
    def chokePeer = copy(peer = peer.copy(chokeStatus = Choked))
    def unchokePeer = copy(peer = peer.copy(chokeStatus = UnChoked))
    def clientInterested = copy(client = client.copy(interestStatus = IsInterested))
    def clientUnInterested = copy(client = client.copy(interestStatus = UnInterested))
    def peerInterested = copy(peer = peer.copy(interestStatus = IsInterested))
    def peerUnInterested = copy(peer = peer.copy(interestStatus = UnInterested))
  }
}

class ClientProtocol
  extends Actor
  with FSM[ClientProtocol.State, ClientProtocol.Data]
  with ActorLogging {

  import ClientProtocol._

  startWith(State(peer = Status(Choked, UnInterested),
                  client = Status(Choked, UnInterested)),
    Uninitialised)

  when(State(peer = Status(Choked, UnInterested),
             client = Status(Choked, UnInterested))) {

    case Event(SetPeer(peer), Uninitialised) => stay() using HasPeer(peer)

    case Event(Send(UnChoke), HasPeer(peer)) => {
      peer ! UnChoke
      goto(stateName.unchokePeer)
    }

    case Event(Send(Interested), HasPeer(peer)) => {
      peer ! Interested
      goto(stateName.clientInterested)
    }
  }

  when(State(peer = Status(UnChoked, UnInterested),
             client = Status(Choked, UnInterested))) {

    case Event(Send(Choke), HasPeer(peer)) => {
      peer ! Choke
      goto(stateName.chokePeer)
    }

    case Event(Send(Interested), HasPeer(peer)) => {
      peer ! Interested
      goto(stateName.clientInterested)
    }

  }

  when(State(peer = Status(Choked, IsInterested),
             client = Status(Choked, UnInterested))) {

    case Event(Send(UnChoke), HasPeer(peer)) => {
      peer ! UnChoke
      goto(stateName.unchokePeer)
    }

    case Event(Send(NotInterested), HasPeer(peer)) => {
      peer ! NotInterested
      goto(stateName.clientUnInterested)
    }

  }

  initialize()
}
