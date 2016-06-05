package io.github.jamespic.regex_keyspace

import org.scalatest.FlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks

/**
  * Created by james on 05/06/16.
  */
class RegexKeyspaceSpec extends FlatSpec with TableDrivenPropertyChecks {
  val validRegexes = Table(
    "regex",
    "a?a",
    "[ab][cd]",
    "Hello (world|you)",
    "Helloo? World",
    "Hello{0,20} World",
    "(H(e|u)llo|Goodbye) World",
    "Hello (there |to you)? world"
  )

  val combinations = Table(
    ("regex", "combinations"),
    ("abc", 1),
    ("[ab]c", 2),
    ("[ab]?c", 3),
    ("\\d", 10),
    ("\\w\\d\\s",1260),
    ("(Hello|Goodbye) (you|world)", 4),
    ("[ab]{4}", 16),
    ("[ab]{0,4}",31)
  )

  "A parsed regex" should "map numbers to strings that match the regex, and back" in {
    forAll(validRegexes) {re =>
      val parsed = RegexAST.parse(re).get
      val scalaRegex = re.r
      for (n <- BigInt(0) until parsed.combinations) {
        val s = parsed.nth(n)
        assert(scalaRegex.unapplySeq(s).isDefined, s"${n}th value of $re was $s, which doesn't match")
        assert(parsed.indexOf(s) == Some(n), s"Expected ${'"'}$s${'"'} to have index $n, actually had ${parsed.indexOf(s)}")
      }
    }
  }

  it should "generate valid regexes at random" in {
    forAll(validRegexes) {re =>
      val parsed = RegexAST.parse(re).get
      val scalaRegex = re.r
      for (_ <- 1 to 20) {
        val s = parsed.random()
        assert(scalaRegex.unapplySeq(s).isDefined, s"$s does not match $re")
      }
    }
  }

  it should "produce valid regexes sequentially" in {
    forAll(validRegexes) {re =>
      val parsed = RegexAST.parse(re).get
      val scalaRegex = re.r
      for (s <- parsed.allMatches) {
        assert(scalaRegex.unapplySeq(s).isDefined, s"$s does not match $re")
      }
    }
  }

  it should "calculate combinations correctly" in {
    forAll(combinations) {(re, combs) =>
      val parsed = RegexAST.parse(re).get
      assertResult(BigInt(combs))(parsed.combinations)
    }
  }
}
