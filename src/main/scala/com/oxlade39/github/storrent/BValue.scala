package com.oxlade39.github.storrent

sealed trait BValue {
  def encode: String
}
case class BInt(value: Int) extends BValue{
  lazy val encode = "i" + value + "e"
}
case class BString(value: String) extends BValue{
  lazy val encode = value.size + ":" + value
}
case class BList(values: BValue*) extends BValue{
  lazy val encode = values.foldLeft("l")((accum, value) => accum + value.encode) + "e"
}
case class BMap(values: Map[BString, BValue]) extends BValue {
  lazy val encode = values.toSeq.sortBy(_._1.value).foldLeft("d")((accum, kv) => accum + kv._1.encode + kv._2.encode) + "e"
}