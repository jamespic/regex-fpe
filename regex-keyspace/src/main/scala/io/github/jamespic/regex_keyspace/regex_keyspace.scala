package io.github.jamespic.regex_keyspace

import java.util.concurrent.ThreadLocalRandom

import scala.collection.Iterable
import scala.util.parsing.combinator._

sealed trait Regex {
  import Regex._
  val combinations: BigInt
  val allMatches: Stream[String]
  def random(): String = selectOne(allMatches)
  def nth(n: BigInt): String
  private[regex_keyspace] def matchPrefix(s: String): Iterable[(BigInt, String)]
  def indexOf(s: String): Option[BigInt] = {
    val potentialMatches = for ((n, "") <- matchPrefix(s)) yield n
    potentialMatches match {
      case List(n) => Some(n)
      case Nil => None
      case _ => throw new NoSuchElementException(s"Regex ${this} has multiple possible matches for $s: $potentialMatches")
    }
  }
}

sealed trait ConcatRegex extends Regex {
  val terms: List[Regex]
  override lazy val combinations = (terms map (_.combinations)).product
  override lazy val allMatches = {
    allMatchesRec(terms)
  }
  override def random() = terms.map(_.random).mkString
  private def allMatchesRec(termList: List[Regex]): Stream[String] = termList match {
    case Nil => Stream("")
    case head :: tail =>
      for (h <- head.allMatches;
           t <- allMatchesRec(tail)) yield h + t
  }
  override def nth(n: BigInt) = nthRec(terms, n)
  private def nthRec(termList: List[Regex], n: BigInt): String = termList match {
    case head :: tail =>
      val (tailN, headN) = n /% head.combinations
      head.nth(headN) + nthRec(tail, tailN)
    case Nil if n == BigInt(0) => ""
    case Nil => throw new NoSuchElementException
  }
  private[regex_keyspace] override def matchPrefix(s: String) = matchPrefixRec(terms, s)
  private def matchPrefixRec(termList: List[Regex], s: String): Iterable[(BigInt, String)] = termList match {
    case head :: tail =>
      for ((n, rest) <- head.matchPrefix(s);
           (m, finalRest) <- matchPrefixRec(tail, rest)) yield (n + head.combinations * m, finalRest)
    case Nil => Iterable((BigInt(0), s))
  }
}

sealed trait OptionRegex extends Regex {
  val options: List[Regex]
  private lazy val offsets = options.scanLeft(BigInt(0)){_ + _.combinations}
  private lazy val optionsZipOffsets = options zip offsets
  override lazy val combinations = (options map (_.combinations)).sum
  override lazy val allMatches = options.toStream flatMap (_.allMatches)
  override def nth(n: BigInt) = nthRec(options, n)
  private def nthRec(opts: List[Regex], n: BigInt): String = opts match {
    case head :: tail if n < head.combinations => head.nth(n)
    case head :: tail => nthRec(tail, n - head.combinations)
    case Nil => throw new NoSuchElementException
  }
  private[regex_keyspace] override def matchPrefix(s: String) = {
    for ((opt, off) <- optionsZipOffsets;
         (n, rest) <- opt.matchPrefix(s)) yield (n + off, rest)
  }
}

final case class Chr(c: Char) extends Regex {
  override val toString = c.toString

  override val combinations: BigInt = BigInt(1)
  override val allMatches: Stream[String] = Stream(toString)
  override val random = toString
  override def nth(n: BigInt) = if (n == BigInt(0)) toString else throw new NoSuchElementException
  private[regex_keyspace] override def matchPrefix(s: String) =
    if (s.startsWith(toString)) (BigInt(0), s.substring(1)) :: Nil else Nil
}

final case class SplitGroup(options: List[Regex]) extends OptionRegex {
  override val toString = options.mkString("(","|",")")
}

final case class ChrSet(options: List[Regex]) extends OptionRegex {
  override val toString = options.mkString("[","","]")
}

final case class ChrRange(from: Chr, to: Chr) extends OptionRegex {
  lazy val options = (from.c to to.c).map(Chr).toList
  override lazy val combinations = to.c - from.c + BigInt(1)
  override lazy val allMatches = Stream.range(from.c, (to.c + 1).toChar) map (_.toString)
  override def random() = (Regex.rand.nextInt(to.c - from.c) + from.c).toChar.toString
  override val toString = from.toString + "-" + to.toString
}

final case class TermList(terms: List[Regex]) extends ConcatRegex {
  override val toString = terms.mkString
}

final case class Opt(x: Regex) extends OptionRegex {
  override val options = List(x, TermList(Nil))
  override val toString = x + "?"
}

final case class Repeat(x: Regex, count: Range) extends OptionRegex {
  override val options = count.map(size => TermList((1 to size).map(_ => x).toList)).toList
  override val toString = s"$x{${if (count.size > 1) count.start + "," + count.end else count.start}"
}

case object DigitChr extends OptionRegex {
  val options = ('0' to '9').toList map Chr
  override val toString = "\\d"
  private[regex_keyspace] override def matchPrefix(s: String) = {
    if (s.length >= 1 && ('0' <= s(0)) && (s(0)) <= '9') {
      Iterable((s(0) - '0', s.substring(1)))
    } else Nil
  }
}

case object WordChr extends OptionRegex {
  val options = List('_') ++ ('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z') map Chr
  override val toString = "\\w"
}

case object SpaceChr extends OptionRegex {
  val options = List(' ', '\t') map Chr
  override val toString = "\\s"
}

class RegexAST extends RegexParsers with JavaTokenParsers {
  def chr = "[A-Za-z0-9 _]".r ^^ {s => Chr(s.charAt(0))}
  def chrRange = chr ~ "-" ~ chr ^^ {case c1 ~ "-" ~ c2 => ChrRange(c1, c2)}
  def chrSet = "[" ~ rep(chrRange | chr) ~ "]" ^^ {case "[" ~ chrs ~ "]" => ChrSet(chrs)}
  def digitChr = "\\d" ^^^ DigitChr
  def wordChr = "\\w" ^^^ WordChr
  def spaceChr = "\\s" ^^^ SpaceChr
  def singleChr = chr | chrSet | digitChr | wordChr | spaceChr
  def group: Parser[SplitGroup] = "(" ~ repsep(termList, "|") ~ ")" ^^ {case "(" ~ chrs ~ ")" => SplitGroup(chrs)}
  def repeatable = singleChr | group
  def option = repeatable ~ "?" ^^ {case x ~ "?" => new Opt(x)}
  def fixedRepeat = repeatable ~ "{" ~ wholeNumber ~ "}" ^^ {case r ~ "{" ~ n ~ "}" => Repeat(r, n.toInt to n.toInt)}
  def variableRepeat = repeatable ~ "{" ~ wholeNumber ~ "," ~ wholeNumber ~ "}" ^^ {
    case r ~ "{" ~ i ~ "," ~ j ~ "}" => Repeat(r, i.toInt to j.toInt)
  }
  def term = option | group | fixedRepeat | variableRepeat | singleChr
  def termList = rep(term) ^^ (TermList(_))
  override def skipWhitespace = false
}


object RegexAST extends RegexAST {
    private val rand = new java.util.Random
    private val doubleMax = BigDecimal(Double.MaxValue).toBigInt
    def parse(s: String): ParseResult[TermList] = parse(termList, s)
    def testParse(s: String) = {
        val res = parse(s).get
        assert(res.toString == s)
        res
    }
}

object Regex {
  protected[regex_keyspace] val rand = ThreadLocalRandom.current
  protected[regex_keyspace] val doubleMax = BigDecimal(Double.MaxValue).toBigInt
  protected[regex_keyspace] def selectOne[T](s: Stream[T]): T = selectOne(s.toList)
  protected[regex_keyspace] def selectOne[T](s: List[T]): T = {
    val size = s.size
    val i = rand.nextInt(size)
    s(i)
  }
  protected[regex_keyspace] def selectWeighted(s: List[Regex]): Regex = {
    val weights = s map {x => (x, x.combinations)}
    val totalWeight = (weights map (_._2)).sum
    if (totalWeight > doubleMax) {
      selectOne(s)
    } else {
      val total = totalWeight.toDouble
      val r = rand.nextDouble * total
      var pos = 0.0
      for ((x, w) <- weights) {
        pos += w.toDouble
        if (pos >= r) return x
      }
      weights.last._1
    }
  }
}