package com.oxlade39.github.storrent

import org.specs2.mutable.Specification
import java.io.File
import com.turn.ttorrent.common.{Torrent => OldTorrent}
import akka.util.ByteString
import java.net.URI

/**
 * @author dan
 */
class TorrentSpec extends Specification {

  "Torrent" should {
    "load name from file" in {

      val testFile: File = file("ubuntu-13.04-desktop-amd64.iso.torrent")
      val torrent: Torrent = Torrent.fromFile(testFile)
      val oldTorrent: OldTorrent = OldTorrent.load(testFile)

      torrent.getName mustEqual oldTorrent.getName
      torrent.getName mustEqual "ubuntu-13.04-desktop-amd64.iso"
    }

    "load comment from file" in {

      val testFile: File = file("ubuntu-13.04-desktop-amd64.iso.torrent")
      val torrent: Torrent = Torrent.fromFile(testFile)
      val oldTorrent: OldTorrent = OldTorrent.load(testFile)

      torrent.getComment mustEqual oldTorrent.getComment
      torrent.comment mustEqual Some("Ubuntu CD releases.ubuntu.com")
    }

    "load createdBy from file" in {

      val testFile: File = file("ubuntu-13.04-desktop-amd64.iso.torrent")
      val torrent: Torrent = Torrent.fromFile(testFile)
      val oldTorrent: OldTorrent = OldTorrent.load(testFile)

      torrent.getCreatedBy mustEqual oldTorrent.getCreatedBy
      torrent.createdBy mustEqual None
    }

    "load file names from file" in {

      val testFile: File = file("ubuntu-13.04-desktop-amd64.iso.torrent")
      val torrent: Torrent = Torrent.fromFile(testFile)
      val oldTorrent: OldTorrent = OldTorrent.load(testFile)

      torrent.getFilenames mustEqual oldTorrent.getFilenames
      torrent.getFilenames mustEqual collection.JavaConversions.seqAsJavaList(List("ubuntu-13.04-desktop-amd64.iso"))
    }

    "calculate infoHash file" in {

      val testFile: File = file("ubuntu-13.04-desktop-amd64.iso.torrent")
      val torrent: Torrent = Torrent.fromFile(testFile)
      val oldTorrent: OldTorrent = OldTorrent.load(testFile)

      torrent.getInfoHash mustEqual oldTorrent.getInfoHash
      torrent.infoHash mustEqual ByteString(-12, 25, -119, -7, 121, 122, -120, 80, 95, -98, 37, -115, 94, 93, 19, 84, -61, 115, 26, -103)
    }

    "calculate size from files" in {

      val testFile: File = file("ubuntu-13.04-desktop-amd64.iso.torrent")
      val torrent: Torrent = Torrent.fromFile(testFile)
      val oldTorrent: OldTorrent = OldTorrent.load(testFile)

      torrent.getSize mustEqual oldTorrent.getSize
      torrent.getSize mustEqual 823132160L
    }

    "calculate tracker count from announce list" in {

      val testFile: File = file("ubuntu-13.04-desktop-amd64.iso.torrent")
      val torrent: Torrent = Torrent.fromFile(testFile)
      val oldTorrent: OldTorrent = OldTorrent.load(testFile)

      torrent.getTrackerCount mustEqual oldTorrent.getTrackerCount
      torrent.getTrackerCount mustEqual 2
    }

    "loads announceList from file" in {

      val testFile: File = file("ubuntu-13.04-desktop-amd64.iso.torrent")
      val torrent: Torrent = Torrent.fromFile(testFile)
      val oldTorrent: OldTorrent = OldTorrent.load(testFile)

      torrent.getAnnounceList mustEqual oldTorrent.getAnnounceList
      torrent.announceList mustEqual List(
        List(new URI("http://torrent.ubuntu.com:6969/announce")),
        List(new URI("http://ipv6.torrent.ubuntu.com:6969/announce")))
    }

    "converts infoHash to hex" in {
      val testFile: File = file("ubuntu-13.04-desktop-amd64.iso.torrent")
      val torrent: Torrent = Torrent.fromFile(testFile)
      val oldTorrent: OldTorrent = OldTorrent.load(testFile)

      torrent.getHexInfoHash mustEqual oldTorrent.getHexInfoHash
      torrent.getHexInfoHash mustEqual "F41989F9797A88505F9E258D5E5D1354C3731A99"
    }

    "single file torrent is not multifile" in {
      val testFile: File = file("ubuntu-13.04-desktop-amd64.iso.torrent")
      val torrent: Torrent = Torrent.fromFile(testFile)

      torrent.isMultifile mustEqual false
    }
    
    "multi file torrent has multiple torrent files" in {
      val testFile: File = file("TP2001 Wallpapers Pack 2k13_01.torrent")
      val torrent: Torrent = Torrent.fromFile(testFile)
      torrent.isMultifile mustEqual true
      torrent.files mustEqual
        TorrentFile("TP2001_Wallpapers_Pack_2k13_00_(Animals_Cities_Landscapes_Mountains_Nature_Poland_Silesia_photos_pictures).zip", 1371642605) ::
        TorrentFile("TP2001_Wallpapers_Pack_2k13_01_(Firefighting_Museum_Myslowice_Slaskie_Poland_muzeum_pozarnictwa_photos_pictures).zip", 12539921) ::
        Nil

    }

  }


  def file(torrentFileName: String): File = {
    new File(Thread.currentThread.getContextClassLoader.getResource(torrentFileName).toURI)
  }
}
