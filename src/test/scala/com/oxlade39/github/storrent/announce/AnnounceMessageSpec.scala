package com.oxlade39.github.storrent.announce

import org.specs2.mutable.Specification
import com.oxlade39.github.storrent._
import java.io.File
import java.net.{InetAddress, URL}
import com.turn.ttorrent.common.protocol.http.{HTTPAnnounceResponseMessage, HTTPAnnounceRequestMessage}
import com.turn.ttorrent.common.protocol.TrackerMessage.AnnounceRequestMessage.RequestEvent
import com.turn.ttorrent.bcodec.{BEValue, BEncoder}
import java.util
import akka.util.ByteString
import java.nio.ByteBuffer
import com.oxlade39.github.storrent.announce.TrackerRequest
import com.oxlade39.github.storrent.BMap
import scala.Some

class AnnounceMessageSpec extends Specification {
  "TrackerRequest" should {
    "build announce URL with params" in {
      val torrent = Torrent.fromFile(torrentFile)
      val peerId: PeerId = PeerId()
      val request = TrackerRequest(
        infoHash = torrent.infoHash,
        peerId = peerId,
        port = 1234,
        uploaded = 500,
        downloaded = 700,
        left = 10000,
        acceptCompact = true,
        noPeerId = false,
        event = Stopped,
        ip = Some(InetAddress.getByName("127.0.0.1"))
      )

      val oldMessage = HTTPAnnounceRequestMessage.craft(
        torrent.infoHash.toArray,
        peerId.encoded.toArray,
        1234,
        500,
        700,
        10000,
        true,
        false,
        RequestEvent.STOPPED,
        "127.0.0.1",
        49)

      val announceUrl: URL = torrent.announceList.flatten.head.toURL
      val withParams: URL = request.appendParams(announceUrl)

      withParams mustEqual oldMessage.buildAnnounceURL(announceUrl)
    }
  }

  "NormalTrackerResponse" should {
    "parse from ByteString with compact peers" in {
      val jmap = new util.HashMap[String, BEValue]()
      jmap.put("interval", new BEValue(20))
      jmap.put("complete", new BEValue(100))
      jmap.put("incomplete", new BEValue(200))


      val ipOne = InetAddress.getByName("127.0.0.1")
      val portOne = 1024.toShort
      val ipTwo = InetAddress.getByName("192.168.0.1")
      val portTwo = 1025.toShort

      val compactPeers = ByteBuffer.allocate(12)
        .put(ipOne.getAddress).putShort(portOne)
        .put(ipTwo.getAddress).putShort(portTwo)
        .array()

      val cp2 = ByteString(compactPeers)

      jmap.put("peers", new BEValue(compactPeers))

      val byteBuffer = BEncoder.bencode(jmap)
//      val bmap = BMap(Map(
//        BBytes(ByteString("interval")) -> BInt(20),
//        BBytes(ByteString("complete")) -> BInt(100),
//        BBytes(ByteString("incomplete")) -> BInt(200),
//        BBytes(ByteString("peers")) -> BBytes(cp2)
//      )).encode

//      val b = BencodeParser2.parse(ByteString(byteBuffer)).get.asInstanceOf[BMap].values(BBytes(ByteString("peers"))) match {
//        case BBytes(bs) => Some(bs.toArray)
//        case _ => None
//      }
//      b.get mustEqual compactPeers

      val oldMessage = HTTPAnnounceResponseMessage.parse(byteBuffer)
      val newMessage = NormalTrackerResponse.unapply(ByteString(byteBuffer.array())).get

      oldMessage.getInterval mustEqual 20
      newMessage.clientRequestInterval mustEqual 20

      oldMessage.getComplete mustEqual 100
      newMessage.numberOfCompletedPeers mustEqual 100

      oldMessage.getIncomplete mustEqual 200
      newMessage.numberOfUncompletedPeers mustEqual 200

      oldMessage.getPeers.get(0).getIp mustEqual "127.0.0.1"
      oldMessage.getPeers.get(0).getPort mustEqual 1024
      oldMessage.getPeers.get(1).getIp mustEqual "192.168.0.1"
      oldMessage.getPeers.get(1).getPort mustEqual 1025

      newMessage.peers.head.address.getAddress.getHostAddress mustEqual "127.0.0.1"
      newMessage.peers.head.port mustEqual 1024
      newMessage.peers.tail.head.address.getAddress.getHostAddress mustEqual "192.168.0.1"
      newMessage.peers.tail.head.port mustEqual 1025
    }
  }

  def torrentFile = {
    val resource = Thread.currentThread().getContextClassLoader.getResource("ubuntu-13.04-desktop-amd64.iso.torrent")
    new File(resource.toURI)
  }
}
