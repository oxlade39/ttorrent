package com.oxlade39.github.storrent.announce

import akka.util.ByteString
import java.net.{InetSocketAddress, URLEncoder, URL, InetAddress}
import com.oxlade39.github.storrent._
import com.turn.ttorrent.bcodec.BDecoder
import java.nio.ByteBuffer
import com.oxlade39.github.storrent.Peer
import scala.Some
import akka.event.slf4j.Logger
import org.slf4j.LoggerFactory

sealed trait Message {
  def urlEncode: String => String = s => URLEncoder.encode(s, Torrent.encoding)
  def urlEncodeB: ByteString => String = b => urlEncode(b.decodeString(Torrent.encoding))

  def appendParams(url: URL): URL
}

sealed trait TrackerRequestEvent {
  def encode: String
}

/**
 * The first request to the tracker must include the event key with this value
 */
case object Started extends TrackerRequestEvent{
  def encode = "started"
}

/**
 * Must be sent to the tracker if the client is shutting down gracefully
 */
case object Stopped extends TrackerRequestEvent{
  def encode = "stopped"
}

/**
 * Must be sent to the tracker when the download completes. However,
 * must not be sent if the download was already 100% complete when the client started.
 * Presumably, this is to allow the tracker to increment the "completed downloads" metric based solely on this event
 */
case object Completed extends TrackerRequestEvent{
  def encode = "completed"
}

case class TrackerRequest(
  infoHash:       ByteString,
  peerId:         PeerId,
  port:           Int,
  uploaded:       Long,
  downloaded:     Long,
  left:           Long,
  acceptCompact:  Boolean,
  noPeerId:       Boolean,
  event:          TrackerRequestEvent,
  ip:             Option[InetAddress] = None,
  numWant:        Int = 50, //Optional. Number of peers that the client would like to receive from the tracker.
  key:            Option[String] = None,
  trackerId:      Option[String] = None
) extends Message {

  def appendParams(url: URL): URL = {
    val builder = new  StringBuilder()
    val externalForm: String = url.toExternalForm

    def appendParam(name: String, value: String) =
      builder.append("&").append(name).append("=").append(value)

    builder.append(externalForm)
    if(externalForm.contains("?") && externalForm.endsWith("?"))
      builder.append("&")
    else
      builder.append("?")

    builder.append("info_hash=").append(urlEncode(infoHash.decodeString(Torrent.encoding)))

    appendParam("peer_id", urlEncodeB(peerId.encoded))
    appendParam("port", port.toString)
    appendParam("uploaded", uploaded.toString)
    appendParam("downloaded", downloaded.toString)
    appendParam("left", left.toString)
    appendParam("compact", (if(acceptCompact) 1 else 0).toString)
    appendParam("no_peer_id", (if(noPeerId) 1 else 0).toString)
    appendParam("event", event.encode)

    ip.map(address => appendParam("ip", address.getHostAddress))
    key.map(k => appendParam("key", k))
    trackerId.map(tid => appendParam("trackerid", tid))

    new URL(builder.toString())
  }
}

sealed trait TrackerResponse extends Message

case class NormalTrackerResponse(
  clientRequestInterval: Int,
  minimumAnnounceInterval: Option[Int] = None,
  trackerId: Option[String] = None,
  numberOfCompletedPeers: Int,
  numberOfUncompletedPeers: Int,
  peers: List[Peer]
) extends TrackerResponse{
  def appendParams(url: URL) = url
}

object NormalTrackerResponse {
  val logger = LoggerFactory.getLogger(NormalTrackerResponse.getClass)

  def unapply(bytes: ByteString): Option[NormalTrackerResponse] = {
    logger.trace("parsing {}", bytes.utf8String)

    BencodeParser2.parse(bytes) match {
      case Some(BMap(values)) if values.contains(BBytes(ByteString("peers"))) => {

        val p1 = values(BBytes(ByteString("peers")))
        val asBytes = p1 match {
          case BBytes(bs) => Some(bs.toArray)
          case _ => None
        }

        println(asBytes.size)

        def bytesToPeers: Array[Byte] => Iterator[Peer] = bytes => bytes.grouped(6).map{ b =>
          val ip = InetAddress.getByAddress(b.take(4).toArray)
          val port = (0xFF & b.drop(4).head) << 8 | (0xFF & b.drop(5).head)
          val address = new InetSocketAddress(ip, port)
          Peer(address)
        }

        def requiredInt(key: String) = values(BBytes(ByteString(key))) match {
          case BInt(i) => Some(i)
          case _ => None
        }

        def optionalInt(key: String) = values.get(BBytes(ByteString(key))) flatMap {
          case BInt(i) => Some(i)
          case _ => None
        }

        def optionalString(key: String) = values.get(BBytes(ByteString(key))) flatMap {
          case BBytes(s) => Some(s.utf8String)
          case _ => None
        }

        for {
          interval <- requiredInt("interval")
          complete <- requiredInt("complete")
          incomplete <- requiredInt("incomplete")
          peers <- asBytes.map(bytesToPeers)
        } yield NormalTrackerResponse(interval,
                                      optionalInt("min interval"),
                                      optionalString("tracker id"),
                                      complete,
                                      incomplete,
                                      peers.toList)
      }
      case x  =>
        println(x)
        None
    }
  }
}

case class WarningTrackerResponse(
  warning: String,
  response: NormalTrackerResponse
) extends TrackerResponse {
  def clientRequestInterval = response.clientRequestInterval
  def minimumAnnounceInterval = response.minimumAnnounceInterval
  def trackerId = response.trackerId
  def numberOfCompletedPeers = response.numberOfCompletedPeers
  def numberOfUncompletedPeers = response.numberOfUncompletedPeers
  def peers = response.peers

  def appendParams(url: URL) = url
}

case class FailureTrackerResponse(failure: String) extends TrackerResponse{
  def appendParams(url: URL) = url
}
