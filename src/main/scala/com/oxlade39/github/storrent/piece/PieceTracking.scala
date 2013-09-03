package com.oxlade39.github.storrent.piece

import akka.actor.{ActorRef, Props, ActorLogging, Actor}
import com.oxlade39.github.storrent.{Piece => Received, _}
import com.oxlade39.github.storrent.Bitfield
import com.oxlade39.github.storrent.Have

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
  case class Downloading(piece: Piece) extends PieceStatus {
    def +(newBlock: Block): PieceStatus = {
      val updatedPiece = piece + newBlock
      if (updatedPiece.isValid)
        Downloaded(updatedPiece)
      else
        Downloading(updatedPiece)
    }
  }
  case class Downloaded(piece: Piece) extends PieceStatus

}

/**
 * I need to
 * <ul>
 *   <li>track which peers have which pieces<li>
 *   <li>determine the least popular pieces</li>
 * <ul>
 *
 * @param torrent
 */
class PieceTracking(torrent: Torrent)
  extends Actor
  with ActorLogging {

  var mappings = PieceMappings()

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

  }
}
