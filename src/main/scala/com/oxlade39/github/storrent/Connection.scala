package com.oxlade39.github.storrent

import akka.actor.{Props, Actor, ActorLogging}
import akka.io.{Tcp, IO}
import java.net.InetSocketAddress
import akka.util.ByteString
import java.util.UUID
import akka.event.LoggingReceive
import scala.collection.immutable.HashSet

class TorrentServer(bindAddress: InetSocketAddress) extends Actor with ActorLogging {

  import context.system

  val peer = Peer(bindAddress)
  var torrents = HashSet.empty[Torrent]

  IO(Tcp) ! Tcp.Bind(self, bindAddress)

  def receive = {
    case TorrentServer.Serve(t) => {
      startServing(t)
      context.become(listening)
    }
  }

  def listening = LoggingReceive {
    case Tcp.Connected(remote, _) => {
      val connection = sender
      val hostAddress = remote.getAddress.getHostAddress

      val connectionHandler =
        context.actorOf(ClientConnectionHandler.props(peer.id, remote, torrents), s"connection-handler-$hostAddress")

      connection ! Tcp.Register(connectionHandler)
    }

    case TorrentServer.Serve(t) => startServing(t)
  }

  def startServing(torrent: Torrent) {
    torrents += torrent
    log.info("now serving {}", torrents)
  }
}

object TorrentServer {

  case class Serve(t: Torrent)

  def props(bindAddress: InetSocketAddress) = Props(new TorrentServer(bindAddress))
}

/**
 * handles incoming connections from clients
 * @param localPeerId the servers peer id
 * @param remoteAddress the clients address
 */
class ClientConnectionHandler(localPeerId: PeerId,
                              remoteAddress: InetSocketAddress,
                              knownTorrents: Set[Torrent])
  extends Actor with ActorLogging {

  lazy val torrentMap = knownTorrents.foldLeft(Map.empty[ByteString, Torrent])(
    (accum, t) => accum + (t.infoHash -> t)
  )
  var buffer: ByteString = ByteString()
  var remotePeer = None: Option[Peer]

  def bytesRequiredForHandshake = Handshake.handshakeSize - buffer.size

  def receive = buffering
  def buffer(data: ByteString): Unit = buffer ++= data

  def buffering: Receive = {
    case Tcp.Received(data) if bytesRequiredForHandshake > data.size => buffer(data)

    case Tcp.Received(data) => {
      val client = sender

      val bytesStillRequired: Int = bytesRequiredForHandshake
      val handshakeBytes = buffer ++ data.take(bytesStillRequired)
      buffer = data.drop(bytesStillRequired)

      def replyWithHandshake(infoHash: ByteString, clientPeerId: PeerId) {
        val reply = Handshake(infoHash, localPeerId)
        client ! Tcp.Write(reply.encoded)
        context.become(established)
        log.info("established connection after successful handshake with {}", clientPeerId)
        remotePeer = Some(Peer(remoteAddress, clientPeerId))
      }

      Handshake.parse(handshakeBytes) match {
        case Some(Handshake(infoHash, clientPeerId)) if torrentMap.contains(infoHash) => {
          log.info("replying to successful handshake from {}", clientPeerId)
          replyWithHandshake(infoHash, clientPeerId)
        }

        case _ => {
          log.warning("Closing client due to bad handshake: {}", handshakeBytes.utf8String)
          client ! Tcp.Close
        }
      }
    }
  }

  def established: Receive = {
    case Tcp.Received(data) => buffer(data)
  }

}

object ClientConnectionHandler {
  def props(localPeerId: PeerId, remoteAddress: InetSocketAddress, knownTorrents: Set[Torrent]) =
    Props(new ClientConnectionHandler(localPeerId, remoteAddress, knownTorrents))
}

object Example extends App{
  val result = Handshake.parse(
    ByteString(19.toByte) ++
      ByteString("BitTorrent protocol") ++
      ByteString(new Array[Byte](8)) ++
      ByteString(new Array[Byte](20)) ++
      ByteString(new Array[Byte](20))
  )

  println(result)
}