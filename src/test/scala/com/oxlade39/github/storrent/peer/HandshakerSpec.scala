package com.oxlade39.github.storrent.peer

import org.specs2.mutable.Specification
import com.oxlade39.github.storrent.test.util.TestFiles
import com.oxlade39.github.storrent.{Peer, PeerId, Torrent}
import akka.util.Timeout
import java.util.concurrent.TimeUnit
import akka.actor.ActorSystem
import com.oxlade39.github.storrent.announce.{NormalTrackerResponse, Started, TrackerRequest, HttpAnnounceClient}
import java.net.URI
import scala.concurrent.Await
import scala.concurrent.duration.Duration

class HandshakerSpec extends Specification with TestFiles {

  // TODO, manual test for now
  skipAll

  "Handshaker" should {
    "attempt handshake with peer" in {

      import akka.pattern.ask
      implicit val timeout = Timeout(60, TimeUnit.SECONDS)
      val sys = ActorSystem("testHttpAnnounceClient")

      val testTorrent = Torrent.fromFile(ubuntuTorrent)


      val announceClient = sys.actorOf(HttpAnnounceClient.props(new URI("http://torrent.ubuntu.com:6969/announce")))
      val localPeerId = PeerId()
      val request = TrackerRequest(testTorrent.infoHash, localPeerId, 6881, 0, 0, testTorrent.getSize,
        acceptCompact = true, noPeerId = false, event = Started)

      val future = (announceClient ? request).mapTo[NormalTrackerResponse]
      val result = Await.result(future, Duration(60, TimeUnit.SECONDS))
      val aPeer: Peer = result.peers.head

      val underTest = sys.actorOf(Handshaker.props(testTorrent.infoHash, localPeerId))

      underTest ! aPeer

      TimeUnit.MINUTES.sleep(10)
    }
  }

}
