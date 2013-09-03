package com.oxlade39.github.storrent.client

import org.specs2.mutable.Specification
import com.oxlade39.github.storrent.test.util.{ActorContext, TestFiles}
import akka.actor.PoisonPill
import com.oxlade39.github.storrent.{Torrent, Peer}
import java.net.InetSocketAddress
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext

class TorrentClientSpec extends Specification with TestFiles {

  import ExecutionContext.Implicits.global

  // TODO, manual test for now
  skipAll

  "TorrentClientSpec" in {
    "Start downloading torrents" in new ActorContext {
      val torrentClient = sys.actorOf(TorrentClient.props(Peer(new InetSocketAddress(0))))

      val torrent = Torrent.fromFile(ubuntuTorrent)
      torrentClient ! TorrentClient.Download(torrent)
      sys.scheduler.scheduleOnce(Duration(5, TimeUnit.MINUTES), torrentClient, TorrentClient.Cancel(torrent))
      sys.scheduler.scheduleOnce(Duration(6, TimeUnit.MINUTES), torrentClient, PoisonPill)

      readLine("Press enter to shutdown")
      torrentClient ! PoisonPill
    }
  }
}
