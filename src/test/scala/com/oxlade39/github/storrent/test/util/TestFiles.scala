package com.oxlade39.github.storrent.test.util

import java.io.File

trait TestFiles {
  def file(torrentFileName: String): File = {
    new File(Thread.currentThread.getContextClassLoader.getResource(torrentFileName).toURI)
  }

  def ubuntuTorrent = file("ubuntu-13.04-desktop-amd64.iso.torrent")
  def multiFileTorrent = file("TP2001 Wallpapers Pack 2k13_01.torrent")
}
