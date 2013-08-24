package com.oxlade39.github.storrent.peer

import akka.actor._
import akka.io.{Tcp, IO}
import com.oxlade39.github.storrent.Peer

object PeerTracking {
  def props = Props(new PeerTracking)

  case class PeersAnnounced(peers: List[Peer])
}

/**
 * Track all of active peers
 */
class PeerTracking
  extends Actor
  with ActorLogging {

  var knownPeers = Set.empty[Peer]

  import context.system
  import PeerTracking._

  def receive = {
    case PeersAnnounced(p) ⇒ {
      p.filterNot(knownPeers.contains) foreach { newPeer =>
        log.debug("received peer we've not seen before {}", p)
        handleNewPeer(newPeer)
      }
      log.info("Now seen {} peers", knownPeers.size)
    }
  }

  def handleNewPeer(peer: Peer)(implicit system: ActorSystem) {
    knownPeers += peer
    IO(Tcp) ! Tcp.Connect(peer.address)
  }
}
