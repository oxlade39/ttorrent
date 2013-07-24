package com.oxlade39.github.storrent

import akka.util.ByteString
import java.nio.ByteBuffer

/**
 * @see https://wiki.theory.org/BitTorrentSpecification
 */
sealed trait Message {
  def length: Int
  def messageId: Option[Int]
  def payload: Option[ByteString]

  private [this] lazy val messageIdBytes: ByteString =
    messageId.map(mId => ByteString(mId.toByte)).getOrElse(ByteString.empty)

  private [this] lazy val payloadBytes: ByteString =
    payload.map(p => p).getOrElse(ByteString.empty)

  lazy val encode: ByteString =
    ByteString(ByteBuffer.allocate(3)) ++ ByteString(length.toByte) ++
    messageIdBytes ++ payloadBytes
}

sealed trait MessageWithoutPayload extends Message {
  val length = 1
  val payload = None
}

case object KeepAlive extends Message {
  val length = 0
  val messageId = None
  val payload = None
}

case object Choke extends MessageWithoutPayload {val messageId = Some(0)}
case object UnChoke extends MessageWithoutPayload {val messageId = Some(1)}
case object Interested extends MessageWithoutPayload {val messageId = Some(2)}

/**
 * <p>
 * The have message is fixed length. The payload is the zero-based index of a piece that has just been
 * successfully downloaded and verified via the hash.
 * </p>
 * <p>
 * Implementer's Note: That is the strict definition, in reality some games may be played.
 * In particular because peers are extremely unlikely to download pieces that they already have,
 * a peer may choose not to advertise having a piece to a peer that already has that piece.
 * At a minimum "HAVE suppression" will result in a 50% reduction in the number of HAVE messages,
 * this translates to around a 25-35% reduction in protocol overhead. At the same time,
 * it may be worthwhile to send a HAVE message to a peer that has that piece already since it will be
 * useful in determining which piece is rare.
 * </p>
 * <p>
 * A malicious peer might also choose to advertise having pieces that it knows the peer will never download.
 * Due to this attempting to model peers using this information is a bad idea.
 * </p>
 */
case class Have(pieceIndex: Int) extends Message {
  val length = 5
  val messageId = Some(4)
  val payload = Some(ByteString(ByteBuffer.allocate(4).putInt(pieceIndex).array()))
}

/**
 * <p>The bitfield message may only be sent immediately after the handshaking sequence is completed,
 * and before any other messages are sent. It is optional, and need not be sent if a client has no pieces.</p>
 * <p>The bitfield message is variable length, where X is the length of the bitfield. The payload is a bitfield
 * representing the pieces that have been successfully downloaded. The high bit in the first byte corresponds
 * to piece index 0. Bits that are cleared indicated a missing piece, and set bits indicate a valid and available
 * piece. Spare bits at the end are set to zero.</p>
 * <p>Some clients (Deluge for example) send bitfield with missing pieces even if it has all data. Then it sends
 * rest of pieces as have messages. They are saying this helps against ISP filtering of BitTorrent protocol. It
 * is called lazy bitfield.</p>
 * <p>A bitfield of the wrong length is considered an error. Clients should drop the connection if they receive
 * bitfields that are not of the correct size, or if the bitfield has any of the spare bits set.</p>
 */
case class Bitfield(bitfield: ByteString) extends Message {
  val payload = Some(bitfield)
  val length = 1 + payload.size
  val messageId = Some(5)
}

case class Request(index: Int, begin: Int, requestLength: Int) extends Message {
  val length = 13
  val messageId = Some(6)
  val payload = Some(ByteString(index.toByte, begin.toByte, requestLength.toByte))
}

case class Piece(pieceIndex: Int, begin: Int, block: ByteString) extends Message {
  val length = 9 + block.size
  val messageId = Some(7)
  val payload = Some(ByteString(pieceIndex.toByte, begin.toByte) ++ block)
}

case class Cancel(index: Int, begin: Int, requestLength: Int) extends Message {
  val length = 13
  val messageId = Some(8)
  val payload = Some(ByteString(index.toByte, begin.toByte, requestLength.toByte))
}

case class Port(port: Int) extends Message {
  val length = 3
  val messageId = Some(9)
  val payload = Some(ByteString(port.toByte))
}