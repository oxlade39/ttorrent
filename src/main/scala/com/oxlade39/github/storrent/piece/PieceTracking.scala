package com.oxlade39.github.storrent.piece

import akka.actor._
import com.oxlade39.github.storrent._
import com.oxlade39.github.storrent.peer.protocol.ClientProtocol
import com.oxlade39.github.storrent.piece.PieceTracking.PieceStatus
import com.oxlade39.github.storrent.Bitfield
import com.oxlade39.github.storrent.Have
import com.oxlade39.github.storrent.piece.PieceTracking.Downloading

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
  case class Downloading(piece: DownloadPiece, downloader: ActorRef) extends PieceStatus {
    def +(newBlock: Block): PieceStatus = {
      val updatedPiece = piece + newBlock
      if (updatedPiece.isValid)
        Downloaded(updatedPiece)
      else
        copy(piece = updatedPiece)
    }
  }
  case class Downloaded(piece: DownloadPiece) extends PieceStatus

}

class PieceTracking(torrent: Torrent)
  extends Actor
  with ActorLogging {

  var mappings = PieceMappings()
  var downloadingPieces = Map.empty[Int, PieceStatus]

  def newPiece(index: Int): DownloadPiece = DownloadPiece(index, torrent.pieceSize, torrent.pieceHashes(index))

  def receive = {
    case Bitfield(haves) ⇒ {
      val peer = sender

      def add: (PieceMappings, Int) ⇒ PieceMappings =
        (existing, piece) ⇒ existing + (peer -> piece)

      mappings = haves.zipWithIndex.filter(_._1).map(_._2).foldLeft(mappings)(add)
    }

    case Have(index) ⇒ {
      mappings + (sender -> index)
    }

    case FSM.Transition(peer, from: ClientProtocol.State, to: ClientProtocol.State) ⇒ {
      (from.peer.chokeStatus, to.peer.chokeStatus) match {
        case (ClientProtocol.Choked, ClientProtocol.UnChoked) ⇒ {
          log.info("peer {} has been unchoked", peer)
          val peerPieces: Option[Set[Int]] = mappings.actorMapping.get(peer)
          val pieceToDL = peerPieces.flatMap { p ⇒
            val notAlreadyDownloading = p.filterNot(downloadingPieces.contains)
            mappings.rarestPieces.find(notAlreadyDownloading.contains)
          }

          pieceToDL foreach { p ⇒
            val toDownload = newPiece(p)
            val downloader = context.actorOf(PieceDownloader.props(peer))
            downloader ! PieceDownloader.Start(toDownload)
            downloadingPieces += (p -> Downloading(toDownload, downloader))
          }
        }
        case (_, _) ⇒ {}
      }
    }

    case PieceDownloader.Done(p) ⇒ {
      log.info("Received notification of completed piece {}", p)
      // TODO handle
    }

  }
}

object PieceDownloader {
  val MAX_PIPELINED_REQUESTS = 5

  def props(peer: ActorRef) = Props(new PieceDownloader(peer))

  sealed trait Message
  case class Start(p: DownloadPiece) extends Message
  case class Stop(p: DownloadPiece) extends Message
  case class Done(p: DownloadPiece) extends Message

  private[PieceDownloader] case class DownloadingPiece(pendingRequestOffsets: List[Int],
                                                       received: DownloadPiece,
                                                       requester: ActorRef)
}

class PieceDownloader(peer: ActorRef,
                      maxPendingRequests: Int = PieceDownloader.MAX_PIPELINED_REQUESTS,
                      requestLength: Int = Request.DEFAULT_REQUEST_SIZE)
  extends Actor
  with ActorLogging {

  def request(pieceIndex: Int, offset: Int) = {
    peer ! Request(pieceIndex, offset)
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
    case PieceDownloader.Stop(toStop) if toStop.index.equals(dl.received.index) ⇒ {
      log.info("stopping download piece {}", toStop.index)
      context.stop(self)
    }

    case Piece(index, begin, block) ⇒ {
      log.info("received offset {} of piece {}", begin, index)
      val updatedDownload = dl.received + Block(block, begin)
      if (updatedDownload.isValid) {
        log.info("finished download piece {} telling {}", index, dl.requester)
        dl.requester ! PieceDownloader.Done(updatedDownload)
        context.stop(self)
      } else {
        val lastDownloadOffset = dl.pendingRequestOffsets.head
        val nextRequestedOffset = lastDownloadOffset + requestLength
        val nextRequests = nextRequestedOffset :: dl.pendingRequestOffsets.filterNot(_ == begin)
        log.info("received block for piece {} at offset {}", index, begin)
        val updated = dl.copy(pendingRequestOffsets = nextRequests, received = updatedDownload)
        context.become(behavior = downloading(updated),
                       discardOld = true)
      }
    }
  }
}