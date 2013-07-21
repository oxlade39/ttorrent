package com.turn.ttorrent.bcodec

import org.specs2.mutable.Specification
import java.io.ByteArrayInputStream

/**
 * @author dan
 */
class BDecoderSpec extends Specification {
  "BDecoder" should {
    "parse bencoded integers in the form i<integer encoded in base ten ASCII>e" in {

      parse("i42e").getInt mustEqual 42
      parse("i0e").getInt mustEqual 0
    }

    "parse bencoded negative integers in the form i-<integer encoded in base ten ASCII>e" in {
      parse("i-42e").getInt mustEqual -42
    }


    "parse bencoded lists in the form l<contents>e" in {
      val input = "l4:spami42ee"

      val output: BEValue = parse(input)
      output.getList.size mustEqual 2
      output.getList.get(0).getString mustEqual "spam"
      output.getList.get(1).getInt mustEqual 42
    }

    "parse bencoded dictionaries in the form d<contents>e" in {
      val input = "d3:bar4:spam3:fooi42ee"

      val output = parse(input).getMap
      output.size mustEqual 2

      output.get("foo").getInt mustEqual 42
      output.get("bar").getString mustEqual "spam"
    }

    "return null for empty strings" in {
      parse("") mustEqual null
    }

    "throw exception for random string" in {
      parse("dsfjd") must throwA[InvalidBEncodingException]
    }

    "throw exception for bad integer format" in {
      parse("ie") must throwA[InvalidBEncodingException]
      parse("i-0e") must throwA[InvalidBEncodingException]
      parse("iAe") must throwA[InvalidBEncodingException]
    }
  }

  def parse(input: String) = BDecoder.bdecode(new ByteArrayInputStream(input.getBytes))
}
