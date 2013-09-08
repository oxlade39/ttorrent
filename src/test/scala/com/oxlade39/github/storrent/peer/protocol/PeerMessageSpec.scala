package com.oxlade39.github.storrent.peer.protocol

import org.specs2.mutable.Specification
import com.oxlade39.github.storrent.{Piece, Request, Message, Bitfield}
import java.util
import com.turn.ttorrent.common.protocol.PeerMessage.BitfieldMessage
import akka.util.ByteString
import akka.io.{PipelinePorts, PipelineFactory}
import org.specs2.mock.Mockito
import org.specs2.matcher.MustMatchers

class PeerMessageSpec extends Specification with Mockito {

  "PeerMessage" should {
    "Round trip Piece message" in {
      RoundTrip(Piece(645, 0, ByteString("hello world")))
    }

    "Round trip Request messages" in {
      RoundTrip(Request(645, 0))
    }
    
    "Decode Bitfield bytes" in {
      val ctx = new HasByteOrder {
        def byteOrder = java.nio.ByteOrder.BIG_ENDIAN
      }

      val input = Bitfield(Seq(
        true, false, true, false, false, false, false, true,
        true, false, false, false, false, false, false, false,
        true, true, true, false, false, false, false, false
      ))

      val PipelinePorts(cmd, evt, mgmt) =
        PipelineFactory.buildFunctionTriple(ctx, new PeerMessageStage)

      val (_, encoded) = cmd(input)
      encoded.iterator.next() mustEqual input.encode
      
      val (decoded, _) = evt(input.encode)
      decoded.iterator.next() mustEqual input

    }

    "Decode BitfieldMessage bytes" in {
      val ctx = new HasByteOrder {
        def byteOrder = java.nio.ByteOrder.BIG_ENDIAN
      }

      val set: util.BitSet = new util.BitSet(8 * 3)

      set.set(0)
      set.set(2)
      set.set(7) // 10100001

      set.set(8) // 10000000

      set.set(16)
      set.set(17)
      set.set(18)// 11100000

      val oldMessage: BitfieldMessage = BitfieldMessage.craft(set)
      val oldAsBytes = oldMessage.getData
      val actualBytes = new Array[Byte](oldAsBytes.remaining())
      oldAsBytes.get(actualBytes)

      val PipelinePorts(cmd, evt, mgmt) =
        PipelineFactory.buildFunctionTriple(ctx, new PeerMessageStage)

      val (msg, bs): (Iterable[Message], Iterable[ByteString]) = evt(ByteString(actualBytes))


      val bitfield = msg.iterator.next().asInstanceOf[Bitfield]
      bitfield.bitfield(0) mustEqual true
      bitfield.bitfield(2) mustEqual true
      bitfield.bitfield(7) mustEqual true
      bitfield.bitfield(8) mustEqual true
      bitfield.bitfield(16) mustEqual true
      bitfield.bitfield(17) mustEqual true
      bitfield.bitfield(18) mustEqual true
    }
  }
}

object RoundTrip extends MustMatchers {
  def apply(msg: Message) = {
    val ctx = new HasByteOrder {
      def byteOrder = java.nio.ByteOrder.BIG_ENDIAN
    }

    val PipelinePorts(cmd, evt, mgmt) =
      PipelineFactory.buildFunctionTriple(ctx, new PeerMessageStage)

    val (inMsgs: Iterable[Message], inBytes: Iterable[ByteString]) = cmd(msg)
    val (outMsgs: Iterable[Message], outBytes: Iterable[ByteString]) = evt(inBytes.head)

    val outMsg: Message = outMsgs.head
    outMsg mustEqual msg
  }
}

