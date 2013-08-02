package com.oxlade39.github.storrent

import akka.util.ByteString

sealed trait BValue {
  def encode: ByteString
}
case class BInt(value: Int) extends BValue{
  lazy val encode = ByteString("i" + value + "e")
}

case class BBytes(value: ByteString) extends BValue{
  def this(s: String) = this(ByteString(s))

  lazy val encode = ByteString(value.size + ":") ++ value
}
object BBytes {
  def apply(s: String): BBytes = apply(ByteString(s))
}

case class BList(values: BValue*) extends BValue{
  val startDelimiter = ByteString("l")
  val endDelimiter = ByteString("e")

  lazy val encode = values.foldLeft(startDelimiter)((accum, value) => accum ++ value.encode) ++ endDelimiter
}
case class BMap(values: Map[BBytes, BValue]) extends BValue {
  val startDelimiter = ByteString("d")
  val endDelimiter = ByteString("e")

  lazy val encode = values.toSeq.sortBy(_._1.value.utf8String).foldLeft(startDelimiter)(
    (accum, kv) => accum ++ kv._1.encode ++ kv._2.encode) ++ endDelimiter
}