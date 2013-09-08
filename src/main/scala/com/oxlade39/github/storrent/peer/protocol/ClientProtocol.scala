package com.oxlade39.github.storrent.peer.protocol

import akka.actor._
import com.oxlade39.github.storrent._
import akka.io.PipelineFactory
import akka.util.ByteString
import scala.util.{Success, Failure, Try}


object ClientProtocol {
  def props(pieceTracking: ActorRef) = Props(new ClientProtocol(pieceTracking))

  sealed trait Data
  case object Uninitialised extends Data
  case class SetPeer(peer: ActorRef)
  case class HasPeer(peer: ActorRef) extends Data

  case class Send(message: Message)
  case class Received(message: Message)

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

    override def toString = s"State(client = $client, peer = $peer)"
  }
}

class ClientProtocol(pieceTracking: ActorRef)
  extends Actor
  with FSM[ClientProtocol.State, ClientProtocol.Data]
  with ActorLogging {

  self ! FSM.SubscribeTransitionCallBack(pieceTracking)

  import ClientProtocol._

  startWith(State(peer = Status(Choked, UnInterested),
                  client = Status(Choked, UnInterested)),
    Uninitialised)

  when(State(peer = Status(Choked, UnInterested),
             client = Status(Choked, UnInterested))) {

    case Event(SetPeer(peer), Uninitialised) ⇒ stay() using HasPeer(peer)

    case Event(Send(UnChoke), HasPeer(peer)) ⇒ {
      peer ! UnChoke
      goto(stateName.unchokePeer)
    }

    case Event(Send(Interested), HasPeer(peer)) ⇒ {
      peer ! Interested
      goto(stateName.clientInterested)
    }

    case Event(Received(Bitfield(pieces)), HasPeer(peer)) ⇒ {
      pieceTracking ! Bitfield(pieces)
      stay()
    }

    case Event(Received(Have(piece)), HasPeer(peer)) ⇒ {
      pieceTracking ! Have(piece)
      stay()
    }

    case Event(Received(UnChoke), HasPeer(peer)) ⇒ {
      log.info("We were UnChoked by our peer")
      goto(stateName.unchokeClient)
    }
  }

  when(State(peer = Status(UnChoked, UnInterested),
             client = Status(Choked, UnInterested))) {

    case Event(Send(Choke), HasPeer(peer)) ⇒ {
      peer ! Choke
      goto(stateName.chokePeer)
    }

    case Event(Send(Interested), HasPeer(peer)) ⇒ {
      peer ! Interested
      goto(stateName.clientInterested)
    }

  }

  when(State(peer = Status(Choked, IsInterested),
             client = Status(Choked, UnInterested))) {

    case Event(Send(UnChoke), HasPeer(peer)) ⇒ {
      peer ! UnChoke
      goto(stateName.unchokePeer)
    }

    case Event(Send(NotInterested), HasPeer(peer)) ⇒ {
      peer ! NotInterested
      goto(stateName.clientUnInterested)
    }

  }

  when(State(peer = Status(Choked, UnInterested),
             client = Status(UnChoked, UnInterested))) {
    case Event(Received(Choke), HasPeer(peer)) ⇒ {
      log.info("We were Choked by our peer")
      goto(stateName.chokeClient)
    }

    case Event(Send(Interested), HasPeer(peer)) ⇒ {
      peer ! Interested
      goto(stateName.clientInterested)
    }

  }

  when(State(peer = Status(Choked, UnInterested),
             client = Status(UnChoked, IsInterested))) {

    case Event(Received(Choke), HasPeer(peer)) ⇒ {
      log.info("We were Choked by our peer")
      goto(stateName.chokeClient)
    }

    case Event(Received(Piece(piece, begin, block)), HasPeer(peer)) ⇒ {
      log.info("received offset {} of piece {}", begin, piece)
      pieceTracking ! Piece(piece, begin, block)
      stay()
    }

    case Event(Send(r: Request), HasPeer(peer)) ⇒ {
      log.info("requesting offset {} of piece {}", r.begin, r.index)
      peer ! r
      stay()
    }
  }

  when(State(peer = Status(Choked, UnInterested),
             client = Status(Choked, IsInterested))) {

    case Event(Send(UnChoke), HasPeer(peer)) ⇒ {
      peer ! UnChoke
      goto(stateName.unchokePeer)
    }

    case Event(Send(NotInterested), HasPeer(peer)) ⇒ {
      peer ! NotInterested
      goto(stateName.clientUnInterested)
    }

    case Event(Received(UnChoke), HasPeer(peer)) ⇒ {
      log.info("We were UnChoked by our peer")
      goto(stateName.unchokeClient)
    }
  }

  whenUnhandled {
    case Event(e, s) ⇒
      log.warning("received unhandled request {} in state {}/{}", e, stateName, s)
      stay()
  }

  initialize()
}

class PeerMessageProcessor(clientProtocol: ActorRef,
                           peerComms: ActorRef)
  extends Actor
  with ActorLogging {

  // register as the peer (facade) with the ClientProtocol
  clientProtocol ! ClientProtocol.SetPeer(self)

  val ctx = new HasByteOrder {
    def byteOrder = java.nio.ByteOrder.BIG_ENDIAN
  }

  val pipeline = PipelineFactory.buildWithSinkFunctions(ctx, new PeerMessageStage)(
    writeToConnection,
    sendToClientProtocol
  )

  def writeToConnection(toWrite: Try[ByteString]) {
    toWrite match {
      case Failure(ex) ⇒ log.error(ex, "Exception in PeerMessageStage pipeline {}", ex.getMessage)
      case Success(bytes) ⇒ peerComms ! PeerComms.Send(bytes)
    }
  }

  def sendToClientProtocol(toSend: Try[Message]) {
    toSend match {
      case Failure(ex) ⇒ log.error(ex, "Exception in PeerMessageStage pipeline {}", ex.getMessage)
      case Success(message) ⇒ clientProtocol ! ClientProtocol.Received(message)
    }
  }

  def receive = {
    case m: Message ⇒ pipeline.injectCommand(m)
    case PeerComms.Received(b) ⇒ pipeline.injectEvent(b)
  }
}
