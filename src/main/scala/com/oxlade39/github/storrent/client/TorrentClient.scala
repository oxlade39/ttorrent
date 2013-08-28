package com.oxlade39.github.storrent.client

import akka.actor._
import com.oxlade39.github.storrent.{Peer, Torrent}
import com.oxlade39.github.storrent.announce.{Announcer, Started}
import com.oxlade39.github.storrent.peer.PeerTracking
import com.oxlade39.github.storrent.announce.TrackerRequest
import scala.Some
import com.oxlade39.github.storrent.peer.protocol.PeerComms

object TorrentClient {
  def props(peer: Peer) = Props(new TorrentClient(peer))

  case class Download(torrent: Torrent)
  case class Cancel(torrent: Torrent)

  /**
   * TODO either think of a better name or move the context into an actor of it's own right
   */
  case class TorrentDownloadContext(announcer: ActorRef, peerTracking: ActorRef) {
    def end(implicit sender: ActorRef = Actor.noSender) = {
      announcer ! Announcer.StopAnnouncing
      peerTracking ! PoisonPill
    }
  }
}

class TorrentClient(clientPeer: Peer) extends Actor with ActorLogging {

  import TorrentClient._

  var downloads = Map.empty[Torrent, TorrentDownloadContext]

  def receive = {
    case Download(t) if !downloads.contains(t) ⇒ {
      log.info("requested torrent {}", t.getName)
      val announcer = newAnnouncer(t)
      downloads += t -> announcer
      log.info("now announcing {} on {}", t.getName, announcer)
    }

    case Cancel(t) ⇒ {
      log.info("cancelling torrent download for {}", t.getName)
      if (downloads.contains(t)) {
          val ctx: TorrentDownloadContext = downloads(t)
          ctx.end
          downloads -= t
      } else
        log.warning("not currently downloading {}", t.getName)
    }
  }

  def newAnnouncer(torrent: Torrent): TorrentDownloadContext = {
    val torrentName = torrent.name

    val peerCommsFactory =
      context.actorOf(PeerComms.factory(torrent, clientPeer), s"peerCommsFactory-$torrentName")

    val peerTracking =
      context.actorOf(PeerTracking.props(5), s"peerTracking-$torrentName")

    peerTracking ! PeerTracking.RegisterConnectedPeerListener(peerCommsFactory)

    val announcer =
      context.actorOf(Announcer.props(peerTracking), s"announcer-$torrentName")

    val request: TrackerRequest = TrackerRequest(
                                    infoHash = torrent.infoHash,
                                    peerId = clientPeer.id,
                                    port = clientPeer.port,
                                    uploaded =  0,
                                    downloaded = 0,
                                    left = torrent.getSize,
                                    acceptCompact = true,
                                    noPeerId = false,
                                    event = Some(Started))
    announcer ! Announcer.StartAnnouncing(request, torrent.announceList)

    TorrentDownloadContext(announcer, peerTracking)
  }

}
