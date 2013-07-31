package com.oxlade39.github.storrent

object BencodeParser {

  def parse(s: String) = parser.parseAll(parser.bvalue, s) match {
    case parser.Success(bencoded, _) => Some(bencoded)
    case _ => None
  }

  private[this] object parser extends scala.util.parsing.combinator.RegexParsers {
    override type Elem = Char

    def positiveInt = """([1-9]\d*)""".r ^^ {
      i => Integer.parseInt(i)
    }

    def integer = """(0|-*[1-9]\d*)""".r ^^ {
      i => Integer.parseInt(i)
    }

    def bint: Parser[BInt] = 'i' ~ integer ~ 'e' ^^ {
      case start ~ i ~ end => BInt(i)
    }

    def length = positiveInt ~ ":"

    def bstring: Parser[BString] = length >> {
      len => """\w{%d}""".format(len._1).r
    } ^^ {
      case s => BString(s)
    }

    def blist: Parser[BList] = 'l' ~ rep(bvalue) ~ 'e' ^^ {
      case l ~ b ~ e => BList(b: _*)
    }

    def bmap: Parser[BMap] = 'd' ~ rep(bstring ~ bvalue) ~ 'e' ^^ {
      case d ~ items ~ e => BMap(items.foldLeft(Map[BString, BValue]()) {
        case (accum, item) =>
          val key: BString = item._1
          val value: BValue = item._2
          accum + (key -> value)
      })
    }

    def bvalue: Parser[BValue] = bint | bstring | blist | bmap
  }
}