package com.oxlade39.github.storrent.peer.protocol

import org.specs2.mutable.Specification
import akka.io.{PipelineFactory, PipelinePorts}
import com.oxlade39.github.storrent.{Message, Piece}
import akka.util.ByteString
import akka.event.NoLogging

/**
 * @author dan
 */
class PipelineSpec extends Specification {
  
  "PeerMessageStage" should {
    "decode messages in sequence" in {
      val ctx = new HasByteOrder {
        def byteOrder = java.nio.ByteOrder.BIG_ENDIAN
        def log = NoLogging
      }

      val PipelinePorts(cmd, evt, mgmt) =
        PipelineFactory.buildFunctionTriple(ctx, new PeerMessageStage)

      val blockZero: ByteString = ByteString("hello ")
      val pieceZero: Piece = Piece(0, 0, blockZero)
      val blockOne: ByteString = ByteString("world")
      val pieceOne: Piece = Piece(0, blockZero.size, blockOne)

      evt(pieceZero.encode.take(5))
      val (msgZero: Iterable[Message], _) = evt(pieceZero.encode.drop(5))
      val (msgOne: Iterable[Message], _) = evt(pieceOne.encode)

      msgZero.head mustEqual pieceZero
      msgOne.head mustEqual pieceOne
    }
  }

}
