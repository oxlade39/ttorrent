package com.turn.ttorrent.common.protocol.http

import org.specs2.mutable.Specification
import com.turn.ttorrent.bcodec.{BEValue, BEncoder}
import java.util
import com.turn.ttorrent.common.protocol.TrackerMessage.MessageValidationException
import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.net.URL

class HttpAnnounceRequestMessageSpec extends Specification {
  "HTTPAnnounceRequestMessage" should {
    "requires info_hash" in {

      val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
      BEncoder.bencode(new util.LinkedHashMap[String, BEValue](), stream)

      HTTPAnnounceRequestMessage.parse(ByteBuffer.wrap(stream.toByteArray)) must throwA[MessageValidationException]

    }
  }
}
