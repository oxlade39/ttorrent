package com.oxlade39.github.storrent

import org.specs2.mutable.Specification

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
      output mustEqual Some(BString("spam"))
    }

    "parse bencoded lists" in {
      val output = BencodeParser.parse("l4:spami42ee")
      output mustEqual Some(BList(BString("spam"), BInt(42)))
    }

    "parse bencoded maps" in {
      val output = BencodeParser.parse("d3:bar4:spam3:fooi42ee")

      output mustEqual Some(BMap(Map(BString("foo") -> BInt(42), BString("bar") -> BString("spam"))))
    }
  }

  "BValue" should {
    "encode" in {
      BMap(Map(BString("foo") -> BInt(42), BString("bar") -> BString("spam"))).encode mustEqual "d3:bar4:spam3:fooi42ee"
    }
  }

}