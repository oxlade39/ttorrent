package com.oxlade39.github.storrent.peer.protocol

import akka.io.{SymmetricPipePair, PipelineContext, SymmetricPipelineStage}
import com.oxlade39.github.storrent._
import akka.util.{ByteIterator, ByteString}
import scala.Some
import akka.event.LoggingAdapter

/**
 * Codecs for bencoding and parsing to torrent messages
 * http://doc.akka.io/docs/akka/2.2.0/scala/io-codec.html
 */
object Pipeline {

}

class BencodeStage extends SymmetricPipelineStage[PipelineContext, BValue, ByteString] {
  override def apply(ctx: PipelineContext) = new SymmetricPipePair[BValue, ByteString] {
    var buffer = None: Option[ByteString]

    override val commandPipeline = { bvalue: BValue ⇒
      ctx.singleCommand(bvalue.encode)
    }

    override val eventPipeline = { bs: ByteString ⇒
      val data = if (buffer.isEmpty) bs else buffer.get ++ bs
      BencodeParser.parse(data) match {
        case Some(bvalue) ⇒ ctx.singleEvent(bvalue)
        case None ⇒ {
          buffer = Some(data)
          Nil
        }
      }
    }
  }
}

/**
 * This trait is used to formualate a requirement for the pipeline context.
 * In this example it is used to configure the byte order to be used.
 */
trait HasByteOrder extends PipelineContext {
  def byteOrder: java.nio.ByteOrder
  def log: LoggingAdapter
}

class PeerMessageStage extends SymmetricPipelineStage[HasByteOrder, Message, ByteString] {
  override def apply(ctx: HasByteOrder) = new SymmetricPipePair[Message, ByteString] {

    implicit val byteOrder = ctx.byteOrder
    
    var buf = None: Option[ByteString]

    private[this] def buffer(data: ByteString) = {
      buf = Some(data)
      ctx.log.debug("buffer now {} bytes", data.size)
      Nil
    }

    private[this] def singleEvent(m: Message, leftOver: Option[ByteString] = None) = {
      buf = leftOver
      ctx.singleEvent(m)
    }

    override val commandPipeline = { message: Message ⇒
      ctx.singleCommand(message.encode)
    }

    override val eventPipeline = { bs: ByteString ⇒

      val data = if (buf.isEmpty) bs else buf.get ++ bs

      val bytes: ByteIterator = data.iterator
      val length = bytes.getLongPart(4).toInt

      if(data.size < length || length == 0) {
        ctx.log.debug("buffering, have {} but need {}", data.size, length)
        buffer(data)
      } else {
        val messageId = bytes.getLongPart(1).toInt

        messageId match {
          case 0 ⇒ singleEvent(Choke)
          case 1 ⇒ singleEvent(UnChoke)
          case 2 ⇒ singleEvent(Interested)
          case 3 ⇒ singleEvent(NotInterested)
          case 4 ⇒ {
            val pieceIndex: Int = bytes.getLongPart(4).toInt
            singleEvent(Have(pieceIndex))
          }
          case 5 ⇒ {
            // TODO
            val xs: Array[Byte] = new Array[Byte](length - 1)
            bytes.getBytes(xs)
            val withoutSignExtention: Array[Int] = xs.map(_.toInt & 0xff)
            val setBits: Seq[Boolean] = BitOps.asBooleans(withoutSignExtention)
            singleEvent(Bitfield(setBits), Some(bytes.toByteString))
          }
          case 6 ⇒ {
            val index: Int = bytes.getLongPart(4).toInt
            val begin: Int = bytes.getLongPart(4).toInt
            val requestLength: Int = bytes.getLongPart(4).toInt
            singleEvent(Request(index, begin, requestLength), Some(bytes.toByteString))
          }
          case 7 ⇒ {
            val pieceIndex: Int = bytes.getLongPart(4).toInt
            val begin: Int = bytes.getLongPart(4).toInt
            val blockBytes = new Array[Byte](length - 9)
            bytes.getBytes(blockBytes)
            singleEvent(Piece(pieceIndex, begin, ByteString(blockBytes)), Some(bytes.toByteString))
          }
          case 8 ⇒ {
            val index: Int = bytes.getLongPart(4).toInt
            val begin: Int = bytes.getLongPart(4).toInt
            val requestLength: Int = bytes.getLongPart(4).toInt
            singleEvent(Cancel(index, begin, requestLength), Some(bytes.toByteString))
          }
          case 9 ⇒ {
            val port = bytes.getShort
            singleEvent(Port(port), None)
          }
          case unknown ⇒ {
            buffer(ByteString())
          }
        }
      }
    }
  }
}