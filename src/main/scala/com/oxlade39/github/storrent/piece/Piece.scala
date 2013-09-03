package com.oxlade39.github.storrent.piece

import akka.util.ByteString
import scala.collection.SortedSet
import java.nio.ByteBuffer
import com.oxlade39.github.storrent.Torrent

object Piece {
  val ordering = Ordering[Int].on[Piece](_.index)
}

object Block {
  val orderByOffset = Ordering[Int].on[Block](block ⇒ block.offset)
}

case class Block(data: ByteString, offset: Int)

case class Piece(
  index: Int,
  size: Int,
  hash: ByteString,
  data: SortedSet[Block] = SortedSet()(Block.orderByOffset)
) {

  def offset = index * size

  def totalBlockSize: Int = data.foldLeft(0)(_ + _.data.size)
  def hasEnoughBytesToBeComplete = totalBlockSize >= size

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

  lazy val isValid: Boolean = contiguousStream.exists(s ⇒ Torrent.hash(s).equals(hash))

  def +(block: Block): Piece = copy(data = data + block)
  def ++(blocks: Set[Block]) = copy(data = data ++ blocks)
}