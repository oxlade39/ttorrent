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

    "requires peer_id" in {

      val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
      val values: util.LinkedHashMap[String, BEValue] = new util.LinkedHashMap[String, BEValue]()
      values.put("info_hash", new BEValue(42))
      BEncoder.bencode(values, stream)

      HTTPAnnounceRequestMessage.parse(ByteBuffer.wrap(stream.toByteArray)) must throwA[MessageValidationException]

    }

    "requires port" in {

      val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
      val values: util.LinkedHashMap[String, BEValue] = new util.LinkedHashMap[String, BEValue]()
      values.put("info_hash", new BEValue(42))
      values.put("peer_id", new BEValue(42))
      BEncoder.bencode(values, stream)

      HTTPAnnounceRequestMessage.parse(ByteBuffer.wrap(stream.toByteArray)) must throwA[MessageValidationException]

    }

    "parses valid message into HTTPAnnounceRequestMessage instance" in {
      val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
      val values: util.LinkedHashMap[String, BEValue] = new util.LinkedHashMap[String, BEValue]()
      values.put("info_hash", new BEValue("hash".getBytes))
      values.put("peer_id", new BEValue("42".getBytes))
      values.put("port", new BEValue(8080))
      values.put("ip", new BEValue("127.0.0.1"))

      BEncoder.bencode(values, stream)

      val output: HTTPAnnounceRequestMessage = HTTPAnnounceRequestMessage.parse(ByteBuffer.wrap(stream.toByteArray))

      output.getInfoHash mustEqual "hash".getBytes
      output.getPeerId mustEqual "42".getBytes
      output.getPort mustEqual 8080
      output.getIp mustEqual "127.0.0.1"
    }

    "builds an announce url from params" in {
      val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
      val values: util.LinkedHashMap[String, BEValue] = new util.LinkedHashMap[String, BEValue]()
      values.put("info_hash", new BEValue("hash".getBytes))
      values.put("peer_id", new BEValue("42".getBytes))
      values.put("port", new BEValue(8080))
      values.put("ip", new BEValue("127.0.0.1"))
      values.put("uploaded", new BEValue(27))
      values.put("downloaded", new BEValue(28))
      values.put("left", new BEValue(100))

      BEncoder.bencode(values, stream)

      val output: HTTPAnnounceRequestMessage = HTTPAnnounceRequestMessage.parse(ByteBuffer.wrap(stream.toByteArray))

      val announceURL: URL = output.buildAnnounceURL(new URL("http://localhost:8081/file"))

      announceURL.toExternalForm mustEqual ("http://localhost:8081/file?" +
        "info_hash=%s&" +
        "peer_id=%s&" +
        "port=%s&" +
        "uploaded=%s&" +
        "downloaded=%s&" +
        "left=%s&" +
        "compact=%s&" +
        "no_peer_id=%s&" +
        "ip=%s").format("hash", "42", 8080, "27", "28", "100", 0, 0, "127.0.0.1")
    }

  }
}
