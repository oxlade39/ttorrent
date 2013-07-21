package com.turn.ttorrent.bcodec

import org.specs2.mutable.Specification
import java.io.ByteArrayOutputStream
import java.util
import java.nio.ByteBuffer

class BEncoderSpec extends Specification {
  "BEncoder" should {
    "encode positive integers" in {
      val out: ByteArrayOutputStream = new ByteArrayOutputStream()
      BEncoder.bencode(49, out)

      val bencoded = new String(out.toByteArray, "UTF-8")

      bencoded mustEqual "i49e"
    }

    "encode strings as bytes arrays" in {
      val out: ByteArrayOutputStream = new ByteArrayOutputStream()
      BEncoder.bencode("spam".asInstanceOf[Any], out)

      val bencoded = new String(out.toByteArray, "UTF-8")

      bencoded mustEqual "4:spam"
    }

    "encode negative integers" in {
      val out: ByteArrayOutputStream = new ByteArrayOutputStream()
      BEncoder.bencode(-49, out)

      val bencoded = new String(out.toByteArray, "UTF-8")

      bencoded mustEqual "i-49e"
    }

    "encode lists" in {
      val out: ByteArrayOutputStream = new ByteArrayOutputStream()
      val list: util.ArrayList[BEValue] = new util.ArrayList[BEValue]()
      list.add(new BEValue("spam"))
      list.add(new BEValue(42))
      BEncoder.bencode(list.asInstanceOf[Any], out)

      val bencoded = new String(out.toByteArray, "UTF-8")

      bencoded mustEqual "l4:spami42ee"
    }

    "encode dictionaries" in {

      val dic = new util.LinkedHashMap[String, BEValue]()
      dic.put("foo", new BEValue(42))
      dic.put("bar", new BEValue("spam"))
      val output: ByteBuffer = BEncoder.bencode(dic)

      val bencoded = new String(output.array(), "UTF-8")

      bencoded mustEqual "d3:bar4:spam3:fooi42ee"
    }
  }
}
