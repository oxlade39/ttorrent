package com.oxlade39.github.storrent

import akka.util.ByteString
import org.slf4j.LoggerFactory

object BencodeParser {

  val logger = LoggerFactory.getLogger(BencodeParser.getClass)

  val digits = List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')

  val identifiers = ('i'.toByte :: 'l'.toByte :: 'd'.toByte :: digits).toSet

  def parse(bytes: ByteString): Option[BValue] = p2(bytes, None).headOption

  def parse(s: String): Option[BValue] = p2(ByteString(s), None).headOption

  private[this] def p2(bytes: ByteString, accum: Option[BValue]): List[BValue] = {
    if (bytes.isEmpty) accum.map(List(_)).getOrElse(Nil)
    else
      bytes.head match {
        case 'i' => parseInt(bytes, accum)
        case d if digits.contains(d.toChar) => parseBinary(bytes, accum)
        case 'l' => parseList(bytes, accum)
        case 'd' => parseDictionary(bytes, accum)

        case _ => {
          if (!bytes.isEmpty) logger.warn("not matched but there are still bytes: {}", bytes.utf8String)
          accum.map(List(_)).getOrElse(Nil)
        }
      }
  }

  private[this] def parseInt(bytes: ByteString, accum: Option[BValue]): List[BValue] = {
    val rest = bytes.tail.takeWhile(b => b != 'e')
    if (rest.equals(ByteString('-'))) Nil
    else if (rest.equals(ByteString("-0"))) Nil
    else {
      val integer = Integer.parseInt(rest.utf8String)
      val leftOver = bytes.drop(2 + rest.size)

      if (accum.isEmpty)
        p2(leftOver, Some(BInt(integer)))
      else
        accum.get :: p2(leftOver, Some(BInt(integer)))
    }
  }

  private[this] def parseBinary(bytes: ByteString, accum: Option[BValue]): List[BValue] = {
    val lengthB = bytes.takeWhile(i => digits.contains(i))
    val length = Integer.parseInt(lengthB.utf8String)
    val bytesSection = bytes.tail.drop(lengthB.size).take(length)
    val leftOver = bytes.tail.drop(lengthB.size + length)
    val bbytes = BBytes(bytesSection)
    if (accum.isEmpty)
      p2(leftOver, Some(bbytes))
    else
      accum.get :: p2(leftOver, Some(bbytes))
  }

  private[this] def parseList(bytes: ByteString, accum: Option[BValue]): List[BValue] = {
    val innerBytes = bytes.tail.drop(1).takeWhile(b => !identifiers.contains(b))
    val rest = bytes.tail.drop(innerBytes.size + 1)
    val values: List[BValue] = p2(ByteString(bytes.tail.head) ++ innerBytes, None) ::: p2(rest.slice(0, rest.size - 1), accum)
    List(BList(values: _*))
  }

  def parseDictionary(bytes: ByteString, accum: Option[BValue]): List[BValue] = {
    logger.trace("d parsing {}", bytes.utf8String)

    val tail: ByteString = bytes.tail
    val inner: List[BValue] = p2(tail.slice(0, tail.size - 1), None)
    val dic = inner.grouped(2).foldLeft(Map.empty[BBytes, BValue]) {
      case (map, List(bs@BBytes(x), value)) => map + (bs -> value)
    }
    List(BMap(dic))
  }

}