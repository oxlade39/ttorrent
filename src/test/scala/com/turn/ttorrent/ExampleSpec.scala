package com.turn.ttorrent

import org.specs2.mutable.Specification
import com.turn.ttorrent.client.{SharedTorrent, Client}
import java.net.InetAddress
import java.io.File

/**
 * @author dan
 */
class ExampleSpec extends Specification {
  "Client" should {
    "download torrents" in {
      // First, instantiate the Client object.
      val client = new Client(
        // This is the interface the client will listen on (you might need something
        // else than localhost here).
        InetAddress.getLocalHost,

        // Load the torrent from the torrent file and use the given
        // output directory. Partials downloads are automatically recovered.
        SharedTorrent.fromFile(
          new File("/path/to/your.torrent"),
          new File("/path/to/output/directory"))
      )

      // At this point, can you either call download() to download the torrent and
      // stop immediately after...
      client.download()

      // Or call client.share(...) with a seed time in seconds:
      // client.share(3600);
      // Which would seed the torrent for an hour after the download is complete.

      // Downloading and seeding is done in background threads.
      // To wait for this process to finish, call:
      client.waitForCompletion()
    }
  }
}
