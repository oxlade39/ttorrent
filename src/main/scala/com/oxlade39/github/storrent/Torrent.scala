package com.oxlade39.github.storrent

import java.io.{ByteArrayInputStream, File}
import akka.util.ByteString
import com.turn.ttorrent.common.ITorrent
import java.net.URI
import scala.io.Source
import com.turn.ttorrent.bcodec.{BEncoder, BEValue, BDecoder}
import java.security.MessageDigest
import java.util.Date


object Torrent {

  val extension = ".torrent"
  val mediaType = "application/x-bittorrent"
  val encoding = "ISO-8859-1"

  def fromFile(file: File): Torrent = {
    val tfr = new TorrentFileReader(file)

    Torrent(
      tfr.name,
      tfr.comment,
      tfr.createdBy,
      tfr.creationDate,
      tfr.files,
      tfr.infoHash,
      tfr.announceList,
      tfr.pieceLength,
      tfr.pieces
    )
  }

  def hash(bytes: ByteString): ByteString = {
    val md = MessageDigest.getInstance("SHA-1")
    md.update(bytes.toArray)
    ByteString(md.digest())
  }

}

class TorrentFileReader(file: File) {
  /** The query parameters encoding when parsing byte strings. */
  val BYTE_ENCODING = "ISO-8859-1"

  import scala.collection.JavaConverters._

  private[this] val source = Source.fromFile(file, BYTE_ENCODING)

  private[this] val (main, info) = try {
    val byteArray = source.map(_.toByte).toArray
    val bdecoded: BEValue = BDecoder.bdecode(new ByteArrayInputStream(byteArray))
    val main: collection.mutable.Map[String, BEValue] = bdecoded.getMap.asScala
    val info: collection.mutable.Map[String, BEValue] = main("info").getMap.asScala

    (main, info)
  } finally {
    source.close()
  }

  val name: String = info("name").getString

  val files: List[TorrentFile] = info.get("files").map(_.getList.asScala).map {
    files ⇒
      files.map {
        file ⇒
          val path = file.getMap.asScala("path").getList.asScala.map(_.getString).mkString(File.separator)
          val size = file.getMap.asScala("length").getLong
          TorrentFile(path, size)
      }.toList
  }.getOrElse(List(TorrentFile(name, info("length").getLong)))

  val announceList = main.get("announce-list").map(_.getList.asScala).map {
    tiers ⇒
      tiers.toList.map(_.getList.asScala.toList.map(uri ⇒ new URI(uri.getString)))
  }.getOrElse {
    List(List(new URI(main("announce").getString)))
  }

  val comment = main.get("comment").map(_.getString)
  val createdBy = main.get("created by").map(_.getString)
  val creationDate = main.get("creation date").map(_.getLong * 1000)
  val infoHash = hash(BEncoder.bencode(info.asJava).array())
  val pieceLength = info("piece length").getInt
  val pieces = info("pieces").getBytes.grouped(20).map(hash => ByteString(hash)).toList

  def hash(byteArray: Array[Byte]) = Torrent.hash(ByteString(byteArray))
}

case class Torrent(name: String,
                   comment: Option[String] = None,
                   createdBy: Option[String] = None,
                   creationTimestamp: Option[Long] = None,
                   files: List[TorrentFile],
                   infoHash: ByteString,
                   announceList: List[List[URI]],
                   pieceSize: Int,
                   pieceHashes: List[ByteString],
                   seeder: Boolean = false) extends ITorrent {

  import collection.JavaConversions._

  def getName = name

  def getComment = comment.getOrElse(null)

  def getCreatedBy = createdBy.getOrElse(null)

  def creationDate = creationTimestamp.map(new Date(_))

  lazy val getSize = files.foldLeft(0L)(_ + _.size)

  def getFilenames = seqAsJavaList(files.map(_.name))

  def isMultifile = files.size > 1

  def getInfoHash = infoHash.toArray

  lazy val getHexInfoHash = infoHash.map("%02X" format _).mkString

  lazy val getAnnounceList = seqAsJavaList(announceList.map(jl ⇒ seqAsJavaList(jl)))

  lazy val getTrackerCount = announceList.flatten.toSet.size

  def isSeeder = seeder

}

case class TorrentFile(name: String, size: Long)
