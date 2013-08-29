package com.oxlade39.github.storrent.piece

import akka.util.ByteString
import java.security.MessageDigest
import scala.collection.SortedSet
import java.nio.ByteBuffer

object Piece {
  def hash(bytes: ByteString): ByteString = {
    val md = MessageDigest.getInstance("SHA-1")
    md.update(bytes.toArray)
    ByteString(md.digest())
  }
}

object Block {
  val orderByOffset = Ordering[Int].on[Block](block ⇒ block.offset)
}

case class Block(data: ByteString, offset: Int)

case class Piece(
  index: Int,
  offset: Long,
  size: Int,
  hash: ByteString,
  data: SortedSet[Block] = SortedSet()(Block.orderByOffset)
) {

  lazy val totalBlockSize: Int = data.foldLeft(0)(_ + _.data.size)
  lazy val hasEnoughBytesToBeComplete = totalBlockSize >= size

  lazy val contiguousStream: Option[ByteString] = {
    val initial: (Option[ByteBuffer], Int) = (Some(ByteBuffer.allocate(size)), 0)
    val result = data.foldLeft(initial){ (accum, block) ⇒
      val (buffer, contiguousBytes) = accum
      if (buffer.isEmpty || contiguousBytes < block.offset)
        (None, 0)
      else {
        val appendedBuffer = buffer.map{ b =>
          b.position(block.offset)
          b.put(block.data.toArray)
          b
        }
        if (appendedBuffer.isDefined)
          (Some(appendedBuffer.get), contiguousBytes + appendedBuffer.get.position())
        else
          (None, 0)
      }
    }
    result._1.map{ buf ⇒
      buf.rewind()
      ByteString(buf)
    }
  }

  lazy val isValid: Boolean = contiguousStream.exists(s ⇒ Piece.hash(s).equals(hash))

  def +(block: Block): Piece = copy(data = data + block)
  def ++(blocks: Set[Block]) = copy(data = data ++ blocks)
}