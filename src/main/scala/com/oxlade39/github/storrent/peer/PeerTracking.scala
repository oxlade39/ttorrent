package com.oxlade39.github.storrent.peer

import akka.actor._
import akka.io.{Tcp, IO}
import com.oxlade39.github.storrent.Peer
import com.oxlade39.github.storrent.peer.PeerTracking.PeerAnnounced

object PeerTracking {
  case class PeerAnnounced(peer: Peer)
}

/**
 * Track all of active peers
 */
class PeerTracking
  extends Actor
  with ActorLogging {

  var knownPeers = Set.empty[Peer]

  import context.system

  def receive = {
    case PeerAnnounced(p) if knownPeers.contains(p) ⇒ {
      log.info("received already known peer {}", p)
    }

    case PeerAnnounced(p) ⇒ {
      log.info("received peer we've not seen before {}", p)
      handleNewPeer(p)
    }
  }

  def handleNewPeer(peer: Peer)(implicit system: ActorSystem) {
    knownPeers += peer
    IO(Tcp) ! Tcp.Connect(peer.address)
  }
}
