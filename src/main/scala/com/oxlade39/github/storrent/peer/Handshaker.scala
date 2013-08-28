package com.oxlade39.github.storrent.peer

import akka.util.ByteString
import com.oxlade39.github.storrent.{Handshake, PeerId}
import akka.actor._
import scala.Some
import com.oxlade39.github.storrent.peer.protocol.PeerComms

object Handshaker {
  def props(infoHash: ByteString, localPeerId: PeerId) = Props(new Handshaker(infoHash, localPeerId))

  case class HandshakeWith(peerComms: ActorRef)

  sealed trait Data
  case object Empty extends Data
  case class Receiving(bytes: ByteString, from: ActorRef) extends Data {

    def bytesRequiredForHandshake = Handshake.handshakeSize - bytes.size

    def buffer(data: ByteString) = copy(bytes = bytes ++ data)
  }

  trait State
  case object Unconnected extends State
  case object HandshakeSent extends State
  case object HandshakeFail extends State
  case object HandshakeSuccess extends State
}

class Handshaker(infoHash: ByteString, peerId: PeerId)
  extends Actor
  with FSM[Handshaker.State, Handshaker.Data]
  with ActorLogging {

  import Handshaker._
  import concurrent.duration._

  startWith(Unconnected, Empty)

  when(Unconnected) {
    case Event(HandshakeWith(peerComms), Empty) ⇒ sendHandshake(peerComms)
  }

  when(HandshakeSent) {
    case Event(PeerComms.Received(data), r @ Receiving(buffer, from))
      if data.size >= r.bytesRequiredForHandshake ⇒ {

      val bytesForHandshake = buffer ++ data.take(r.bytesRequiredForHandshake)
      val leftOver = data.drop(r.bytesRequiredForHandshake)

      Handshake.parse(bytesForHandshake) match {
        case Some(success) ⇒  {
          log.info("successful handshake {}", success)
          goto(HandshakeSuccess) using r.copy(bytes = leftOver)
        }
        case None ⇒ {
          log.error("handshaking failed. bytesForHandshake: [{}] leftOver: [{}]", bytesForHandshake.utf8String, leftOver.utf8String)
          goto(HandshakeFail) using r.copy(bytes = leftOver)
        }
      }
    }
  }

  when(HandshakeSuccess, stateTimeout = 1.milli) {
    case Event(StateTimeout, data) => stop(FSM.Normal, data)
  }

  when(HandshakeFail)(FSM.NullFunction)

  onTransition {
    case _ -> HandshakeFail ⇒ stateData match {
      case Receiving(data, peer) ⇒ {
        log.warning("HandshakeFail with data {} so closing connection", data.utf8String)
      }
      case _ ⇒ log.warning("HandshakeFail before started receiving")
    }
    case a -> b ⇒ log.debug("transition {} -> {}", a, b)
  }

  whenUnhandled {
    case Event(event, data) ⇒
      log.warning("Received unhandled event: {} in {} with {}", event, stateName, data)
      stay()
  }

  onTermination {
    case StopEvent(FSM.Normal, state, data)         ⇒ log.info("normal stop")
    case StopEvent(FSM.Shutdown, state, data)       ⇒ log.info("shutdown stop")
    case StopEvent(FSM.Failure(cause), state, data) ⇒ log.warning("failure stop")
  }

  initialize()

  def sendHandshake(peer: ActorRef) = {
    val handshake: ByteString = Handshake(infoHash, peerId).encoded
    log.debug("sending handshake {} to {}", handshake.utf8String, peer)
    peer ! PeerComms.Send(handshake)
    log.debug("handshake sent")
    goto(HandshakeSent) using Receiving(ByteString.empty, sender)
  }
}