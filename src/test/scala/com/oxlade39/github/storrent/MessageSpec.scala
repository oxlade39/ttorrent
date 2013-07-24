package com.oxlade39.github.storrent

import org.specs2.mutable.Specification
import com.turn.ttorrent.common.protocol.PeerMessage._
import com.turn.ttorrent.common.protocol.PeerMessage
import akka.util.ByteString

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

  def test(peerMessage: PeerMessage, message: Message) = {
    val messageByteBuffer = message.encode.asByteBuffer
    val peerMessageByteBuffer = peerMessage.getData

    message.encode.size mustEqual ByteString(peerMessageByteBuffer).size
    peerMessageByteBuffer.rewind()

    messageByteBuffer mustEqual peerMessageByteBuffer
  }
}
