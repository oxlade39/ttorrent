package com.oxlade39.github.storrent

import java.io.File
import akka.util.ByteString


case class Torrent(file: File) {

  // TODO read/generate from file
  lazy val infoHash = ByteString.empty

}
