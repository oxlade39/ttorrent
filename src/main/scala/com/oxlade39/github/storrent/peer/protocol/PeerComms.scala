package com.oxlade39.github.storrent.peer.protocol

import akka.actor._
import akka.util.ByteString
import akka.io.Tcp
import com.oxlade39.github.storrent.peer.{Handshaker, PeerTracking}
import com.oxlade39.github.storrent.Torrent
import com.oxlade39.github.storrent.Peer

/**
 * Starts awaiting a connected peer,
 * then attempts handshake,
 * if successful begins the bittorrent client/peer communication phase
 */
class PeerComms(torrent: Torrent,
                localPeer: Peer,
                pieceTracking: ActorRef)
  extends Actor 
  with ActorLogging {
  
  def receive = awaitingConnection
  
  def awaitingConnection: Receive = {
    case PeerTracking.ConnectedPeer(connectedPeer, connection) ⇒ {
      connection ! Tcp.Register(self)
      val handshaker = context.actorOf(Handshaker.props(torrent.infoHash, localPeer.id), "handshaker")
      handshaker ! FSM.SubscribeTransitionCallBack(self)
      handshaker ! Handshaker.HandshakeWith(self)
      log.info("now handshaking")
      context.become(handshaking(connection, handshaker))
    }
  }
  
  def handshaking(peerTcpConnection: ActorRef, handshaker: ActorRef): Receive = 
    forwardTo(peerTcpConnection, handshaker).orElse {
      
    case FSM.Transition(_, oldState, Handshaker.HandshakeSuccess) ⇒ {
      val clientProc = context.actorOf(ClientProtocol.props(pieceTracking), "clientProc")
      val peerProtocol = context.actorOf(Props(new PeerMessageProcessor(clientProc, self)), "peerProtocol")
      log.info("now established")
      context.become(established(peerTcpConnection, peerProtocol))
    }

    case FSM.Transition(_, oldState, Handshaker.HandshakeFail) ⇒ {
      log.warning("closing Tcp connection with {} due to handshake failure", peerTcpConnection)
      peerTcpConnection ! Tcp.Close
      log.warning("stopping self as handshake failed and connection with peer closed")
      context.stop(self)
    }
  }
  
  def established(peerTcpConnection: ActorRef, peerProtocol: ActorRef) = 
    forwardTo(peerTcpConnection, peerProtocol)
  
  def forwardTo(peerTcpConnection: ActorRef, recipient: ActorRef): Receive = {
    case Tcp.Received(bytes) ⇒ {
      recipient ! PeerComms.Received(bytes)
    }
    case PeerComms.Send(toSend) ⇒ peerTcpConnection ! Tcp.Write(toSend)
  }
}

object PeerComms {
  /**
   * Creates a new PeerComms Actor on receiving a PeerTracking.ConnectedPeer message
   */
  def factory(torrent: Torrent, localPeer: Peer, pieceTracking: ActorRef) = Props(new Actor with ActorLogging {
    def receive = {
      case connected: PeerTracking.ConnectedPeer ⇒ {
        val address = connected.peer.hostAddress
        val comms =
          context.actorOf(props(torrent, localPeer, pieceTracking), s"peerComms-$address")
        comms.forward(connected)
      }
    }
  })

  def props(torrent: Torrent, localPeer: Peer, pieceTracking: ActorRef) =
    Props(new PeerComms(torrent, localPeer, pieceTracking))

  case class Send(toSend: ByteString)
  case class Received(bytes: ByteString)
}