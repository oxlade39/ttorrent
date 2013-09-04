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
      val blockOne: Block = Block(blockOneBytes, 0)
      val blockTwo: Block = Block(blockTwoBytes, blockOneBytes.size)

      SortedSet(blockOne, blockTwo)(Block.orderByOffset).head mustEqual blockOne
      SortedSet(blockTwo, blockOne)(Block.orderByOffset).head mustEqual blockOne
    }
  }

  "Piece" should {
    "be a collection of overlapping blocks" in {
      val expectedData = ByteString("Hello World")

      val hello = ByteString("Hello Mars")
      val blockZero = Block(hello, 0)
      val world = ByteString(" World")
      val blockOne = Block(data = world, offset = ByteString("Hello").size)

      val piece =
        DownloadPiece(0, expectedData.size, Torrent.hash(expectedData)) ++ Set(blockZero, blockOne)

      piece.contiguousStream mustEqual Some(expectedData)
    }
  }
}
