package com.oxlade39.github.storrent.announce

import org.specs2.mutable.Specification
import com.oxlade39.github.storrent.{PeerId, Torrent}
import java.io.File
import java.net.{InetAddress, URL}
import com.turn.ttorrent.common.protocol.http.HTTPAnnounceRequestMessage
import com.turn.ttorrent.common.protocol.TrackerMessage.AnnounceRequestMessage.RequestEvent

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

  def torrentFile = {
    val resource = Thread.currentThread().getContextClassLoader.getResource("ubuntu-13.04-desktop-amd64.iso.torrent")
    new File(resource.toURI)
  }
}
