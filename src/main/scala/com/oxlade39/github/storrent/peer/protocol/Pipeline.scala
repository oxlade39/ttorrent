package com.oxlade39.github.storrent.peer.protocol

import akka.io.{SymmetricPipePair, PipelineContext, SymmetricPipelineStage}
import com.oxlade39.github.storrent._
import akka.util.{ByteIterator, ByteString}
import scala.Some

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
}

class PeerMessageStage extends SymmetricPipelineStage[HasByteOrder, Message, ByteString] {
  override def apply(ctx: HasByteOrder) = new SymmetricPipePair[Message, ByteString] {

    implicit val byteOrder = ctx.byteOrder
    
    var buffer = None: Option[ByteString]

    override val commandPipeline = { message: Message ⇒
      ctx.singleCommand(message.encode)
    }

    override val eventPipeline = { bs: ByteString ⇒

      val data = if (buffer.isEmpty) bs else buffer.get ++ bs

      val bytes: ByteIterator = data.iterator
      val length = bytes.getLongPart(4).toInt

      if(data.size < length) {
        buffer = Some(data)
        Nil
      } else {
        val messageId = bytes.getLongPart(1).toInt

        messageId match {
          case 0 ⇒ ctx.singleEvent(Choke)
          case 1 ⇒ ctx.singleEvent(UnChoke)
          case 2 ⇒ ctx.singleEvent(Interested)
          case 3 ⇒ ctx.singleEvent(NotInterested)
          case 4 ⇒ {
            val pieceIndex: Int = bytes.getLongPart(4).toInt
            ctx.singleEvent(Have(pieceIndex))
          }
          case 5 ⇒ {
            // TODO
            val xs: Array[Byte] = new Array[Byte](length - 1)
            bytes.getBytes(xs)
            val withoutSignExtention: Array[Int] = xs.map(_.toInt & 0xff)
            val setBits: Seq[Boolean] = BitOps.asBooleans(withoutSignExtention)
            ctx.singleEvent(Bitfield(setBits))
          }
          case 6 ⇒ {
            val index: Int = bytes.getLongPart(4).toInt
            val begin: Int = bytes.getLongPart(4).toInt
            val requestLength: Int = bytes.getLongPart(4).toInt
            ctx.singleEvent(Request(index, begin, requestLength))
          }
          case 7 ⇒ {
            val pieceIndex: Int = 0
            val begin: Int = 0
            val blockBytes = new Array[Byte](length - 9)
            bytes.getBytes(blockBytes)
            ctx.singleEvent(Piece(pieceIndex, begin, ByteString(blockBytes)))
          }
          case 8 ⇒ {
            val index: Int = bytes.getLongPart(4).toInt
            val begin: Int = bytes.getLongPart(4).toInt
            val requestLength: Int = bytes.getLongPart(4).toInt
            ctx.singleEvent(Cancel(index, begin, requestLength))
          }
          case 9 ⇒ {
            val port = bytes.getShort
            ctx.singleEvent(Port(port))
          }
          case unknown ⇒ {
            buffer = Some(data)
            Nil
          }
        }
      }
    }
  }
}