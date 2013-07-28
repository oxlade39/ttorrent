package com.oxlade39.github.storrent

import akka.util.ByteString
import java.util.UUID
import java.net.InetSocketAddress

case class Peer(address: InetSocketAddress, id: PeerId = PeerId()) {
  def hostAddress = address.getAddress.getHostAddress
  def port = address.getPort
}

case class PeerId(id: String) {
  lazy val encoded: ByteString = ByteString(id, Torrent.encoding)
}

object PeerId {
  val BITTORRENT_ID_PREFIX = "-TO0042-"

  def randomId = BITTORRENT_ID_PREFIX + UUID.randomUUID().toString.split("-")(4)

  def apply(id: ByteString): PeerId = PeerId(id.decodeString(Torrent.encoding))
  def apply(): PeerId = PeerId(randomId)
}
