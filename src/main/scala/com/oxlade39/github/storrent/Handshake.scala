package com.oxlade39.github.storrent

import akka.util.ByteString

case class Handshake(infoHash: ByteString, peerId: PeerId) {
  lazy val encoded: ByteString =
    ByteString(Handshake.BitTorrentProtocolString.length.toByte) ++
      ByteString(Handshake.BitTorrentProtocolString) ++
      ByteString(new Array[Byte](8)) ++
      infoHash ++
      peerId.encoded

  override def toString = "Handshake(\"" + infoHash.utf8String + "\"," + peerId.toString + ")"
}

object Handshake {
  val BitTorrentProtocolString = "BitTorrent protocol"
  private [Handshake] val reservedOffsetSize = 8
  private [Handshake] val torrentInfoHashSize = 20
  private [Handshake] val peerIdSize = 20

  private [Handshake] val protocolNameOffset = 1
  private [Handshake] val reservedOffset = protocolNameOffset + BitTorrentProtocolString.length
  private [Handshake] val infoHashOffset = reservedOffset + reservedOffsetSize
  private [Handshake] val peerIdOffset = infoHashOffset + torrentInfoHashSize

  val handshakeSize = 1 + BitTorrentProtocolString.length + reservedOffsetSize + torrentInfoHashSize + peerIdSize

  def parse(bs: ByteString): Option[Handshake] = {
    if(bs.size < handshakeSize)
      None
    else {
      val protocolStringLength = bs.slice(0, protocolNameOffset)
      val protocolString = bs.slice(protocolNameOffset, reservedOffset)
      val reserved = bs.slice(reservedOffset, infoHashOffset)
      val infoHash = bs.slice(infoHashOffset, peerIdOffset)
      val peerId = bs.slice(peerIdOffset, peerIdOffset + peerIdSize)

      val equals: Boolean = protocolStringLength.equals(ByteString(BitTorrentProtocolString.length.toByte))
      if(equals &&
        protocolString.equals(ByteString(BitTorrentProtocolString)))
        Some(Handshake(infoHash, PeerId(peerId)))
      else
        None
    }

  }
}
