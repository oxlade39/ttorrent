package com.oxlade39.github.storrent.peer

import akka.actor._
import com.oxlade39.github.storrent.Peer
import akka.io.Tcp
import akka.io

object PeerTracking {
  def props(maxConnections: Int = 30) = Props(new PeerTracking(maxConnections))

  case class PeersAnnounced(peers: List[Peer])
  case class RegisterConnectedPeerListener(listener: ActorRef)

  sealed trait TrackedPeer {
    def peer: Peer
  }
  case class ConnectedPeer(peer: Peer, connection: ActorRef) extends TrackedPeer
  case class AwaitingConnectionPeer(peer: Peer) extends TrackedPeer {
    def connected(con: ActorRef) = ConnectedPeer(peer, con)
  }
  case class UnconnectedPeer(peer: Peer) extends TrackedPeer

  private[PeerTracking] def isConnected(trackedPeer: TrackedPeer) = trackedPeer match {
    case ConnectedPeer(_, _) ⇒ true
    case _ ⇒ false
  }

  private[PeerTracking] def isPending(trackedPeer: TrackedPeer) = trackedPeer match {
    case AwaitingConnectionPeer(_) ⇒ true
    case _ ⇒ false
  }
}

/**
 * Track all of active peers
 */
class PeerTracking(maxConnections: Int)
  extends Actor
  with ActorLogging {

  import PeerTracking._

  var trackedPeers = Map.empty[Peer, TrackedPeer]
  def allKnownPeers = trackedPeers.keys.toSet

  private[this] def connectedPeers = trackedPeers.values.filter(isConnected)
  private[this] def unconnectedPeers = trackedPeers.values.filterNot(isConnected)
  private[this] def awaitingConnection = trackedPeers.values.filter(isPending)

  var listeners = List.empty[ActorRef]

  import context.system
  import PeerTracking._

  def receive = {
    case PeersAnnounced(p) ⇒ {
      val knownPeers = allKnownPeers
      p.filterNot(knownPeers.contains) foreach { newPeer ⇒
        log.debug("received peer we've not seen before {}", p)
        handleNewPeer(newPeer)
      }
      log.info("Now seen {} peers", knownPeers.size)
    }

    case Tcp.Connected(remoteAddress, localAddress) ⇒ {
      val peerConnection = sender
      log.info("{} is now connected on {} by {}", remoteAddress, localAddress, peerConnection)
      awaitingConnection.find(_.peer.address.equals(remoteAddress)) map { trackedPeer ⇒
        log.info("{} mapped to {}", peerConnection, trackedPeer)
        trackedPeers -= trackedPeer.peer
        val connectedPeer = ConnectedPeer(trackedPeer.peer, peerConnection)
        trackedPeers += trackedPeer.peer -> connectedPeer
        notifyListeners(connectedPeer)
      }
    }

    case RegisterConnectedPeerListener(listener) ⇒ listeners = listener :: listeners
  }

  def handleNewPeer(peer: Peer)(implicit system: ActorSystem) {
    val connectedOrWaiting = connectedPeers.size + awaitingConnection.size
    if (connectedOrWaiting < maxConnections) {
      log.info("currently have {} connections from a max of {} so connecting to {}",
        connectedOrWaiting, maxConnections, peer)

      io.IO(Tcp) ! Tcp.Connect(peer.address)
      trackedPeers += peer -> AwaitingConnectionPeer(peer)
    } else {
      log.info("too many connections so adding new peer {} to backlog", peer)
      trackedPeers += peer -> UnconnectedPeer(peer)
    }
  }

  def notifyListeners(peer: ConnectedPeer) {
    log.info("sending {} to listeners {}", peer, listeners)
    listeners.foreach(_.tell(peer, self))
  }
}
