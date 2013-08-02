package com.oxlade39.github.storrent

import org.specs2.mutable.Specification
import akka.util.ByteString

class BencodeParserSpec extends Specification {
  "BencodeParser" should {
    "parse bencoded integers" in {
      val output = BencodeParser.parse("i42e")
      output mustEqual Some(BInt(42))
    }

    "parse bencoded negative integers" in {
      val output = BencodeParser.parse("i-42e")
      output mustEqual Some(BInt(-42))
    }

    "not parse bencoded negative zero" in {
      val output = BencodeParser.parse("i-e")
      output mustEqual None
    }

    "parse bencoded strings" in {
      val output = BencodeParser.parse("4:spam")
      output mustEqual Some(BBytes("spam"))
    }

    "parse bencoded lists" in {
      val output = BencodeParser.parse("l4:spami42ee")
      output mustEqual Some(BList(BBytes("spam"), BInt(42)))
    }

    "parse bencoded maps" in {
      val output = BencodeParser.parse(ByteString("d3:bar4:spam3:fooi42ee"))

      output mustEqual Some(BMap(Map(BBytes("foo") -> BInt(42),
                                     BBytes("bar") -> BBytes("spam"))))
    }
  }

  "BValue" should {
    "encode" in {
      val encoded = BMap(Map(BBytes("foo") -> BInt(42),
                             BBytes("bar") -> BBytes("spam")))
                            .encode
      encoded.utf8String mustEqual "d3:bar4:spam3:fooi42ee"
    }
  }

}