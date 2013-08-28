package com.oxlade39.github.storrent

import org.specs2.mutable.Specification

/**
 * @author dan
 */
class BitOpsSpec extends Specification {
  "BitOps" should {
    "indicate if bit i is set" in {
      BitOps.isSet(1.toByte, 0) mustEqual true
      BitOps.isSet(1.toByte, 1) mustEqual false

      BitOps.isSet(2.toByte, 0) mustEqual false
      BitOps.isSet(2.toByte, 1) mustEqual true
      BitOps.isSet(2.toByte, 2) mustEqual false
    }

    "have implicit for is bit set" in {
      import BitOps.richByte

      1.toByte.isSet(0) mustEqual true
      1.toByte.isSet(1) mustEqual false
    }

    "work with streams of bits" in {
      // [161,128,224]
      val seq =
        Seq(161, // 10100001
            128, // 10000000
            224) // 11100000

      BitOps.asBooleans(seq) mustEqual Seq(
        true, false, true, false, false, false, false, true,   //10100001
        true, false, false, false, false, false, false, false, //10000000
        true, true, true, false, false, false, false, false    //11100000
      )
    }
  }
}
