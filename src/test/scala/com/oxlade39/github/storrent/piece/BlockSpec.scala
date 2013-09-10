package com.oxlade39.github.storrent.piece

import org.specs2.mutable.Specification
import scala.collection.SortedSet
import akka.util.ByteString
import com.oxlade39.github.storrent.Torrent

/**
 * @author dan
 */
class BlockSpec extends Specification {
  "Block" should {
    "be sortable" in {
      val blockOneBytes: ByteString = ByteString("one")
      val blockTwoBytes: ByteString = ByteString("two")
      val blockOne: Block = Block(0, blockOneBytes)
      val blockTwo: Block = Block(blockOneBytes.size, blockTwoBytes)

      SortedSet(blockOne, blockTwo)(Block.orderByOffset).head mustEqual blockOne
      SortedSet(blockTwo, blockOne)(Block.orderByOffset).head mustEqual blockOne
    }
  }

  "DownloadPiece" should {
    "be a collection of overlapping blocks" in {
      val expectedData = ByteString("Hello World")

      val hello = ByteString("Hello Mars")
      val blockZero = Block(0, hello)
      val world = ByteString(" World")
      val blockOne = Block(data = world, offset = ByteString("Hello").size)

      val piece =
        DownloadPiece(0, expectedData.size, Torrent.hash(expectedData)) + blockZero + blockOne

      val stream: piece.contiguousStream.type = piece.contiguousStream
      stream mustEqual Some(expectedData)
    }
  }
}
