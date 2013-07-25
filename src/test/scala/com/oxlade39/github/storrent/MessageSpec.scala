package com.oxlade39.github.storrent

import org.specs2.mutable.Specification
import com.turn.ttorrent.common.protocol.PeerMessage._
import com.turn.ttorrent.common.protocol.PeerMessage
import akka.util.ByteString
import java.util
import java.nio.{CharBuffer, ByteBuffer}

class MessageSpec extends Specification {
  "KeepAlive" should {
    "encode" in {
      test(KeepAliveMessage.craft(), KeepAlive)
    }
  }

  "Choke" should {
    "encode" in {
      test(ChokeMessage.craft(), Choke)
    }
  }

  "UnChoke" should {
    "encode" in {
      test(UnchokeMessage.craft(), UnChoke)
    }
  }

  "Interested" should {
    "encode" in {
      test(InterestedMessage.craft(), Interested)
    }
  }

  "Have" should {
    "encode" in {
      test(HaveMessage.craft(500), Have(500))
    }
  }

  "Bitfield" should {
    "encode" in {
      val set: util.BitSet = new util.BitSet(20)
      set.set(0)
      set.set(2)
      set.set(7)
      set.set(8)
      set.set(16)
      set.set(17)
      set.set(18)
      test(BitfieldMessage.craft(set), Bitfield(0.to(19).map(_ => false))
                                          .set(0)
                                          .set(2)
                                          .set(7)
                                          .set(8)
                                          .set(16)
                                          .set(17)
                                          .set(18))
    }
  }

  def test(peerMessage: PeerMessage, message: Message) = {
    val messageByteBuffer = message.encode.asByteBuffer
    val peerMessageByteBuffer = peerMessage.getData

    message.encode.size mustEqual ByteString(peerMessageByteBuffer).size
    peerMessageByteBuffer.rewind()

    val leftCharBuffer: CharBuffer = messageByteBuffer.asCharBuffer()
    val left: Array[Char] = new Array[Char](leftCharBuffer.length())
    leftCharBuffer.get(left)


    val rightCharBuffer: CharBuffer = peerMessageByteBuffer.asCharBuffer
    val right: Array[Char] = new Array[Char](rightCharBuffer.length())
    rightCharBuffer.get(right)

    left.map(_.toInt) mustEqual right.map(_.toInt)
  }
}
