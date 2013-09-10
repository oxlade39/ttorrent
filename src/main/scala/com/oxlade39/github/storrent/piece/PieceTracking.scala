package com.oxlade39.github.storrent.piece

import akka.actor._
import com.oxlade39.github.storrent._
import com.oxlade39.github.storrent.peer.protocol.ClientProtocol
import com.oxlade39.github.storrent.piece.PieceTracking.PieceStatus
import com.oxlade39.github.storrent.Bitfield
import com.oxlade39.github.storrent.Have
import com.oxlade39.github.storrent.piece.PieceTracking.Downloading
import scala.collection.GenTraversable
import com.oxlade39.github.storrent.peer.protocol.ClientProtocol.ChokeStatus

case class PieceMappings(actorMapping: Map[ActorRef, Set[Int]] = Map.empty) {

  lazy val rarestPieces: List[Int] = {
    val emptyMappings: Map[Int, Int] = Map.empty[Int, Int].withDefaultValue(0)
    val piecesWithTotalCount = actorMapping.foldLeft(emptyMappings){ (pieceToCount, mappings) ⇒
      val (_, pieces) = mappings
      val updatedCounts = pieces.foldLeft(pieceToCount) { (accum2, p) ⇒ accum2 + (p -> (accum2(p) + 1))}
      updatedCounts
    }
    piecesWithTotalCount.keys.toList.sortBy(pieceIndex ⇒ piecesWithTotalCount(pieceIndex))
  }

  def +(kv: (ActorRef, Int)): PieceMappings = {
    val (actor, hasPiece) = kv
    val currentMappings = actorMapping.get(actor).getOrElse(Set.empty[Int])
    PieceMappings(actorMapping + (actor ->  (currentMappings + hasPiece)))
  }

  def -(kv: (ActorRef, Int)): PieceMappings = {
    val (actor, doesNotHavePiece) = kv
    val currentMappings = actorMapping.get(actor).getOrElse(Set.empty[Int])
    PieceMappings(actorMapping + (actor ->  (currentMappings - doesNotHavePiece)))
  }
}


object PieceTracking {
  def props(torrent: Torrent) = Props(new PieceTracking(torrent))

  sealed trait PieceStatus
  case class Downloading(piece: DownloadPiece, downloader: ActorRef, peer: ActorRef) extends PieceStatus {
    def +(newBlock: Block): PieceStatus =
      copy(piece = piece + newBlock)
  }
  case class Downloaded(piece: Int) extends PieceStatus

}

class PieceTracking(torrent: Torrent)
  extends Actor
  with ActorLogging {

  var mappings = PieceMappings()
  var downloadingPieces = Map.empty[Int, PieceStatus]

  def newPiece(index: Int): DownloadPiece = DownloadPiece(index, torrent.pieceSize, torrent.pieceHashes(index))

  def finishedPieces = downloadingPieces.values.flatMap {
    case PieceTracking.Downloaded(i) ⇒ Some(i)
    case _ ⇒ None
  }.toSet

  def notifyInterested(piecesThisPeerHas: GenTraversable[Int], peer: ActorRef) = {
    val wantedPieces = piecesThisPeerHas.filterNot(finishedPieces.contains)
    if (wantedPieces.isEmpty) {
      peer ! ClientProtocol.Send(NotInterested)
    } else {
      peer ! ClientProtocol.Send(Interested)
    }
  }

  def receive = {
    case Bitfield(haves) ⇒ {
      val peer = sender

      def add: (PieceMappings, Int) ⇒ PieceMappings =
        (existing, piece) ⇒ existing + (peer -> piece)

      val piecesThisPeerHas: Seq[Int] = haves.zipWithIndex.filter(_._1).map(_._2)
      mappings = piecesThisPeerHas.foldLeft(mappings)(add)

      notifyInterested(piecesThisPeerHas, peer)
    }

    case Have(index) ⇒ {
      mappings += (sender -> index)
      notifyInterested(Seq(index), sender)
    }

    case p: Piece ⇒ {
      val index: Int = p.pieceIndex
      val pieceStatus: Option[PieceStatus] = downloadingPieces.get(index)
      pieceStatus match {
        case Some(Downloading(piece, downloader, peer)) ⇒ downloader.forward(p)
        case other ⇒ log.warning("received piece we didn't request {}", p)
      }
    }

    case FSM.Transition(peer, from: ClientProtocol.State, to: ClientProtocol.State) ⇒ {

      val BeenUnChoked = ClientProtocol.Choked -> ClientProtocol.UnChoked
      val BeenChoked = ClientProtocol.UnChoked -> ClientProtocol.Choked

      val previousChokeStatus: ChokeStatus = from.client.chokeStatus
      val newChokeStatus: ChokeStatus = to.client.chokeStatus
      (previousChokeStatus, newChokeStatus) match {
        case BeenUnChoked ⇒ {
          log.debug("peer {} has been unchoked", peer)
          downloadNextRarestPiece(peer)
        }

        case BeenChoked ⇒ {
          val download = downloadingPieces.values.find {
            case PieceTracking.Downloading(_, _, dlPeer) ⇒ dlPeer.equals(peer)
            case _ ⇒ false
          }.asInstanceOf[Option[PieceTracking.Downloading]]
          download.map(_.downloader ! PieceDownloader.Stop)
        }

        case _ ⇒ {}
      }
    }

    case PieceDownloader.Done(p) ⇒ {
      log.info("Received notification of completed piece {}", p)
      // TODO save them
      downloadingPieces += (p.index -> PieceTracking.Downloaded(p.index))
      log.info("now finished pieces {}", finishedPieces.mkString(","))
      downloadNextRarestPiece(sender)
    }

  }

  def downloadNextRarestPiece(peer: ActorRef) {
    val peerPieces: Option[Set[Int]] = mappings.actorMapping.get(peer)
    val pieceToDL = peerPieces.flatMap {
      p ⇒
        val notAlreadyDownloading = p.filterNot(downloadingPieces.contains)
        mappings.rarestPieces.find(notAlreadyDownloading.contains)
    }

    pieceToDL foreach {
      p ⇒
        val toDownload = newPiece(p)
        val downloader = context.actorOf(PieceDownloader.props(peer, torrent.pieceSize), s"downloadingPiece-$p")
        downloader ! PieceDownloader.Start(toDownload)
        downloadingPieces += (p -> Downloading(toDownload, downloader, peer))
    }
  }
}

object PieceDownloader {
  val MAX_PIPELINED_REQUESTS = 5

  def props(peer: ActorRef, pieceSize: Int) = Props(new PieceDownloader(peer, pieceSize))

  sealed trait Message
  case class Start(p: DownloadPiece) extends Message
  case object Stop extends Message
  case class Done(p: DownloadPiece) extends Message

  private[PieceDownloader] case class DownloadingPiece(pendingRequestOffsets: List[Int],
                                                       received: DownloadPiece,
                                                       requester: ActorRef)
}

class PieceDownloader(peer: ActorRef,
                      pieceSize: Int,
                      maxPendingRequests: Int = PieceDownloader.MAX_PIPELINED_REQUESTS,
                      requestLength: Int = Request.DEFAULT_REQUEST_SIZE)
  extends Actor
  with ActorLogging {

  def request(pieceIndex: Int, offset: Int, length: Int =  requestLength) = {
    peer ! ClientProtocol.Send(Request(pieceIndex, offset, length))
  }

  def receive = {

    case PieceDownloader.Start(p) ⇒ {
      log.info("starting download of {}", p)
      val pendingRequestOffsets = 0.until(maxPendingRequests).reverse.map { offsetIndex ⇒
        val offset = requestLength * offsetIndex
        request(p.index, offset)
        offset
      }
      val piece = PieceDownloader.DownloadingPiece(pendingRequestOffsets.toList, p, sender)
      context.become(behavior = downloading(piece),
                     discardOld = true)
    }
  }

  def downloading(dl: PieceDownloader.DownloadingPiece): Receive = {
    case PieceDownloader.Stop ⇒ {
      log.debug("stopping download of piece {}", dl.received.index)
      dl.pendingRequestOffsets.foreach { offset ⇒
        peer ! ClientProtocol.Send(Cancel(dl.received.index, requestLength * offset, requestLength))
      }
      context.stop(self)
    }

    case Piece(index, begin, block) ⇒ {
      log.debug("received offset {} of piece {}", begin, index)
      val updatedDownload = dl.received + Block(begin, block)
      if (updatedDownload.isValid) {
        log.info("finished download piece {} telling {}", index, dl.requester)
        dl.requester.tell(PieceDownloader.Done(updatedDownload), sender)
        context.stop(self)
      } else {
        val lastDownloadOffset = dl.pendingRequestOffsets.head
        val bytesRemaining = pieceSize - lastDownloadOffset
        val nextRequestedOffset = lastDownloadOffset + requestLength

        val length = Math.min(requestLength, bytesRemaining)

        if(!updatedDownload.hasEnoughBytesToBeComplete) {
          log.debug("sending next request for offset {}", nextRequestedOffset)
          request(dl.received.index, nextRequestedOffset, length)
          val nextRequests = nextRequestedOffset :: dl.pendingRequestOffsets.filterNot(_ == begin)
          val updated = dl.copy(pendingRequestOffsets = nextRequests, received = updatedDownload)
          context.become(behavior = downloading(updated),
                         discardOld = true)
        } else {
          log.warning("reached end of requests but isn't valid")
        }

      }
    }
  }
}