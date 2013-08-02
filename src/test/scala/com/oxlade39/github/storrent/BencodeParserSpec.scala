package com.oxlade39.github.storrent

import org.specs2.mutable.Specification
import akka.util.ByteString

class BencodeParserSpec extends Specification {
  "BencodeParser" should {
    "parse bencoded integers" in {
      val output = BencodeParser.parse(ByteString("i42e"))
      output mustEqual Some(BInt(42))
    }

    "parse bencoded negative integers" in {
      val output = BencodeParser.parse(ByteString("i-42e"))
      output mustEqual Some(BInt(-42))
    }

    "not parse bencoded negative zero" in {
      val output = BencodeParser.parse(ByteString("i-e"))
      output mustEqual None
    }

    "parse bencoded strings" in {
      val output = BencodeParser.parse(ByteString("4:spam"))
      output mustEqual Some(BBytes(ByteString("spam")))
    }

    "parse bencoded lists" in {
      val output = BencodeParser.parse(ByteString("l4:spami42ee"))
      output mustEqual Some(BList(BBytes(ByteString("spam")), BInt(42)))
    }

    "parse bencoded maps" in {
      val output = BencodeParser.parse(ByteString("d3:bar4:spam3:fooi42ee"))

      output mustEqual Some(BMap(Map(BBytes(ByteString("foo")) -> BInt(42),
                                     BBytes(ByteString("bar")) -> BBytes(ByteString("spam")))))
    }
  }

  "BValue" should {
    "encode" in {
      val encoded: String = BMap(Map(BBytes(ByteString("foo")) -> BInt(42),
                                     BBytes(ByteString("bar")) -> BBytes(ByteString("spam"))))
                            .encode
      encoded mustEqual "d3:bar4:spam3:fooi42ee"
    }
  }

}