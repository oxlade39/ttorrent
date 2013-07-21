package com.turn.ttorrent.bcodec

import org.specs2.mutable.Specification
import java.io.ByteArrayOutputStream

class BEncoderSpec extends Specification {
  "BEncoder" should {
    "encode positive integers" in {
      val out: ByteArrayOutputStream = new ByteArrayOutputStream()
      BEncoder.bencode(49, out)

      val bencoded = new String(out.toByteArray, "UTF-8")

      bencoded mustEqual "i49e"
    }

    "encode negative integers" in {
      val out: ByteArrayOutputStream = new ByteArrayOutputStream()
      BEncoder.bencode(-49, out)

      val bencoded = new String(out.toByteArray, "UTF-8")

      bencoded mustEqual "i-49e"
    }
  }
}
