package com.oxlade39.github.storrent.peer.protocol

import akka.actor._
import com.oxlade39.github.storrent.{Choke, UnChoke, Peer}
import akka.io.Tcp
import com.oxlade39.github.storrent.Peer
import akka.io

/**
 * @author dan
 */
class PeerConnection(peer: Peer)
  extends Actor
  with FSM[PeerConnection.State, PeerConnection.Data]
  with ActorLogging {

  import PeerConnection._
  import concurrent.duration._
  import context._

  io.IO(Tcp) ! Tcp.Connect(peer.address)

  startWith(AttemptingToConnect, NoConnection)

  when(AttemptingToConnect) {
    case Event(Tcp.Connected(remote, local), NoConnection) => {
      log.info("{} is now connected with {} on {}", peer, remote, local)
      goto(Connected) using ConnectedWith(sender)
    }

    case Event(Tcp.Aborted | Tcp.Closed | Tcp.CommandFailed | Tcp.ErrorClosed | Tcp.PeerClosed, NoConnection) => {
      log.info("{} errored while waiting for connection response", peer)
      goto(Disconnected)
    }
  }

  when(Connected) {
    case Event(Tcp.Aborted | Tcp.Closed | Tcp.CommandFailed | Tcp.ErrorClosed | Tcp.PeerClosed, NoConnection) => {
      log.info("{} errored while connected", peer)
      goto(Disconnected)
    }
  }

  when(Disconnected, 30.seconds) {
    case Event(StateTimeout, data) => {
      log.info("{} connection actor is stopping after disconnection", peer)
      stop()
    }
  }

  onTransition {
    case a -> b => log.info("{} transitioning from {} to {}", self, a, b)
  }

  initialize()
}

object PeerConnection {
  def props(peer: Peer) = Props(new PeerConnection(peer))

  sealed trait Data

  case object NoConnection extends Data
  case class ConnectedWith(connection: ActorRef) extends Data

  sealed trait State

  case object AttemptingToConnect extends State
  case object Connected extends State
  case object Disconnected extends State
}