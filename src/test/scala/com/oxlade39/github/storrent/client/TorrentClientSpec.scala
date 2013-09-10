package com.oxlade39.github.storrent.client

import org.specs2.mutable.Specification
import com.oxlade39.github.storrent.test.util.{ActorContext, TestFiles}
import akka.actor.PoisonPill
import com.oxlade39.github.storrent.{Torrent, Peer}
import java.net.InetSocketAddress

class TorrentClientSpec extends Specification with TestFiles {


  // TODO, manual test for now
  skipAll

  "TorrentClientSpec" in {
    "Start downloading torrents" in new ActorContext {
      val torrentClient = sys.actorOf(TorrentClient.props(Peer(new InetSocketAddress(0))))

      val torrent = Torrent.fromFile(ubuntuTorrent)
      torrentClient ! TorrentClient.Download(torrent)

      readLine("Press enter to shutdown")

      torrentClient ! TorrentClient.Cancel(torrent)
      torrentClient ! PoisonPill
    }
  }
}
