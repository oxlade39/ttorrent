package com.oxlade39.github.storrent.peer.protocol

import akka.actor.{ActorRef, FSM, ActorLogging, Actor}
import com.oxlade39.github.storrent.{Choke, UnChoke, Peer}

/**
 * @author dan
 */
class PeerConnection
  extends Actor
  with FSM[PeerConnection.PeerConnectionState, PeerConnection.Data]
  with ActorLogging {

  import PeerConnection._

  startWith(AmChokingPeerChoking, Uninitialized)

  when(AmChokingPeerChoking) {
    case Event(cp @ ConnectedPeer(p), Uninitialized) => stay using cp
    case Event(UnChoke, cp @ ConnectedPeer(peer)) => goto(AmChokingPeerInterested) using cp
  }

  when(AmInterestedPeerChoking) {
    case Event(Choke, cp @ ConnectedPeer(peer)) => {
      peer ! Choke
      goto(AmChokingPeerChoking) using cp
    }
  }

  when(AmInterestedPeerInterested) {
    case Event(Choke, cp @ ConnectedPeer(peer)) => {
      peer ! Choke
      goto(AmChokingPeerInterested) using cp
    }
  }

}

object PeerConnection {
  sealed class PeerConnectionState(client: State, peer: State)

  sealed trait Data
  case object Uninitialized extends Data
  case class ConnectedPeer(peer: ActorRef) extends Data

  sealed trait Side
  object ClientSide extends Side
  object PeerSide extends Side

  sealed trait ClientPeerState

  case object Choked extends ClientPeerState
  case object Interested extends ClientPeerState

  sealed trait State {
    def side: Side
    def state: ClientPeerState
  }

  /**
   *  this client is choking the peer
   */
  case object AmChoking extends State {
    def side = PeerSide
    def state = Choked
  }

  /**
   * this client is interested in the peer
   */
  case object AmInterested extends State {
    def side = ClientSide
    def state = Interested
  }

  /**
   * peer is choking this client
   */
  case object PeerChoking extends State {
    def side = ClientSide
    def state = Choked
  }

  /**
   * peer is interested in this client
   */
  case object PeerInterested extends State {
    def side = PeerSide
    def state = Interested
  }

  case object AmChokingPeerChoking extends PeerConnectionState(AmChoking, PeerChoking)
  case object AmChokingPeerInterested extends PeerConnectionState(AmChoking, PeerInterested)

  case object AmInterestedPeerChoking extends PeerConnectionState(AmInterested, PeerChoking)
  case object AmInterestedPeerInterested extends PeerConnectionState(AmInterested, PeerInterested)

}