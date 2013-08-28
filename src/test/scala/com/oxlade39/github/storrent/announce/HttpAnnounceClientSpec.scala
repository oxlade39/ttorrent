package com.oxlade39.github.storrent.announce

import org.specs2.mutable.Specification
import java.net.URI
import com.oxlade39.github.storrent.{PeerId, Torrent}
import com.oxlade39.github.storrent.test.util.{ActorContext, TestFiles}
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import akka.util.Timeout

class HttpAnnounceClientSpec extends Specification with TestFiles {

  "HttpAnnounceClient" should {
    "request uri" in new ActorContext {

      import akka.pattern.ask
      implicit val timeout = Timeout(60, TimeUnit.SECONDS)

      val underTest = sys.actorOf(HttpAnnounceClient.props(new URI("http://torrent.ubuntu.com:6969/announce")))

      val testTorrent = Torrent.fromFile(ubuntuTorrent)

      val request = TrackerRequest(testTorrent.infoHash, PeerId(), 6881, 0, 0, testTorrent.getSize,
                                   acceptCompact = true, noPeerId = false, event = Some(Started))

      val future = (underTest ? request).mapTo[NormalTrackerResponse]
      val result = Await.result(future, Duration(60, TimeUnit.SECONDS))
      result.peers.size must be_>(1)
    }
  }
}


