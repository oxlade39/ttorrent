package com.oxlade39.github.storrent.announce

import akka.util.ByteString
import java.net.{InetSocketAddress, URLEncoder, URL, InetAddress}
import com.oxlade39.github.storrent.{BitOps, Torrent, PeerId, Peer}
import com.turn.ttorrent.bcodec.BDecoder
import java.nio.ByteBuffer

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
  def unapply(bytes: ByteString): Option[NormalTrackerResponse] = {
    import collection.JavaConversions._

    val decoded = BDecoder.bdecode(bytes.toByteBuffer)
    val asMap = decoded.getMap.toMap
    if(!asMap.containsKey("peers")) None
    else {

      val asBytes = asMap("peers").getBytes
      val peers = asBytes.grouped(6).map{ b =>
        val ip = InetAddress.getByAddress(b.take(4).toArray)
        val port = (0xFF & b.drop(4).head) << 8 | (0xFF & b.drop(5).head)
        val address = new InetSocketAddress(ip, port)
        Peer(address)
      }


      Some(NormalTrackerResponse(
        asMap("interval").getInt,
        asMap.get("min interval").map(_.getInt),
        asMap.get("tracker id").map(_.getString),
        asMap("complete").getInt,
        asMap("incomplete").getInt,
        peers.toList
      ))
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
