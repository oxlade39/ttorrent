package com.oxlade39.github.storrent

import org.specs2.mutable.Specification
import java.io.File
import com.turn.ttorrent.common.{Torrent => OldTorrent}

/**
 * @author dan
 */
class TorrentSpec extends Specification {

  "Torrent" should {
    "load name from file" in {

      val testFile: File = new File(Thread.currentThread.getContextClassLoader.getResource("ubuntu-13.04-desktop-amd64.iso.torrent").toURI)
      val torrent: Torrent = Torrent.fromFile(testFile)
      val oldTorrent: OldTorrent = OldTorrent.load(testFile)

      torrent.getName mustEqual oldTorrent.getName
    }

    "load comment from file" in {

      val testFile: File = new File(Thread.currentThread.getContextClassLoader.getResource("ubuntu-13.04-desktop-amd64.iso.torrent").toURI)
      val torrent: Torrent = Torrent.fromFile(testFile)
      val oldTorrent: OldTorrent = OldTorrent.load(testFile)

      torrent.getComment mustEqual oldTorrent.getComment
    }

    "load createdBy from file" in {

      val testFile: File = new File(Thread.currentThread.getContextClassLoader.getResource("ubuntu-13.04-desktop-amd64.iso.torrent").toURI)
      val torrent: Torrent = Torrent.fromFile(testFile)
      val oldTorrent: OldTorrent = OldTorrent.load(testFile)

      torrent.getCreatedBy mustEqual oldTorrent.getCreatedBy
    }

    "load file names from file" in {

      val testFile: File = new File(Thread.currentThread.getContextClassLoader.getResource("ubuntu-13.04-desktop-amd64.iso.torrent").toURI)
      val torrent: Torrent = Torrent.fromFile(testFile)
      val oldTorrent: OldTorrent = OldTorrent.load(testFile)

      torrent.getFilenames mustEqual oldTorrent.getFilenames
    }

    "calculate infoHash file" in {

      val testFile: File = new File(Thread.currentThread.getContextClassLoader.getResource("ubuntu-13.04-desktop-amd64.iso.torrent").toURI)
      val torrent: Torrent = Torrent.fromFile(testFile)
      val oldTorrent: OldTorrent = OldTorrent.load(testFile)

      torrent.getInfoHash mustEqual oldTorrent.getInfoHash
    }

    "calculate size from files" in {

      val testFile: File = new File(Thread.currentThread.getContextClassLoader.getResource("ubuntu-13.04-desktop-amd64.iso.torrent").toURI)
      val torrent: Torrent = Torrent.fromFile(testFile)
      val oldTorrent: OldTorrent = OldTorrent.load(testFile)

      torrent.getSize mustEqual oldTorrent.getSize
    }

    "calculate tracker count from announce list" in {

      val testFile: File = new File(Thread.currentThread.getContextClassLoader.getResource("ubuntu-13.04-desktop-amd64.iso.torrent").toURI)
      val torrent: Torrent = Torrent.fromFile(testFile)
      val oldTorrent: OldTorrent = OldTorrent.load(testFile)

      torrent.getTrackerCount mustEqual oldTorrent.getTrackerCount
    }

    "loads announceList from file" in {

      val testFile: File = new File(Thread.currentThread.getContextClassLoader.getResource("ubuntu-13.04-desktop-amd64.iso.torrent").toURI)
      val torrent: Torrent = Torrent.fromFile(testFile)
      val oldTorrent: OldTorrent = OldTorrent.load(testFile)

      torrent.getAnnounceList mustEqual oldTorrent.getAnnounceList
    }

    "converts infoHash to hex" in {
      val testFile: File = new File(Thread.currentThread.getContextClassLoader.getResource("ubuntu-13.04-desktop-amd64.iso.torrent").toURI)
      val torrent: Torrent = Torrent.fromFile(testFile)
      val oldTorrent: OldTorrent = OldTorrent.load(testFile)

      torrent.getHexInfoHash mustEqual oldTorrent.getHexInfoHash
    }
  }

}
