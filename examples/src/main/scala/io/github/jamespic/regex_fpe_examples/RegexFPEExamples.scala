package io.github.jamespic.regex_fpe_examples

import io.github.jamespic.regex_fpe.Encrypter
import net._95point2.fpe.FPE
import java.time.{LocalDate, YearMonth}
import java.math.BigInteger

/**
 * Encrypt a Postcode, using a generic postcode matcher to generate a realistic
 * looking postcode.
 */
object PostcodeEncrypter extends Encrypter("(GIR ?0AA|[A-PR-UWYZ]([0-9]{1,2}|([A-HK-Y][0-9]([0-9ABEHMNPRV-Y])?)|[0-9][A-HJKPS-UW]) ?[0-9][ABD-HJLNP-UW-Z]{2})")
/**
 * Encrypt a name - really naive
 */
object NameEncrypter extends Encrypter("[A-Z][a-z]{0,20}")

trait CycleWalkingEncrypter extends Encrypter {
  protected def stripChecksum(s: String): String
  protected def reattachChecksum(s: String): Option[String]
  override def encrypt(message: String, key: Array[Byte], tweak: Array[Byte]) = {
    val stripped = stripChecksum(message)
    walkCycle(stripped, super.encrypt(_, key, tweak))
  }
  override def decrypt(message: String, key: Array[Byte], tweak: Array[Byte]) = {
    val stripped = stripChecksum(message)
    walkCycle(stripped, super.decrypt(_, key, tweak))
  }
  private def walkCycle(message: String, action: String => String): String = {
    val result = action(message)
    reattachChecksum(result) match {
      case Some(msg) => msg
      case None => walkCycle(result, action)
    }
  }
}

/**
 * Encrypt NHS number, making sure that the resulting number has a valid check
 * digit, using cycle walking.
 */
object NHSNumberEncrypter extends Encrypter("\\d{9}") with CycleWalkingEncrypter {
  protected override def stripChecksum(s: String) = s.replace("-", "").substring(0, 9)
  protected override def reattachChecksum(s: String) = {
    val numbers = s map (_ - '0')
    val checksum = 11 - (numbers zip (10 to 2 by -1) map {case (x, y) => x * y}).sum % 11
    val checkDigit = checksum match {
      case 11 => Some("0")
      case 10 => None
      case x => Some(x.toString)
    }
    checkDigit map (digit => (s.grouped(3)).mkString("-") + digit)
  }
}

/**
 * Only encrypt the day, not the month or year, to allow age based uses. Uses
 * month and year as a tweak value, so days will encrypt differently in different
 * months.
 */
object BirthDateEncrypter {
  val ThirtyTwo = BigInteger.valueOf(32) // Use 32 for month length, because 31 is prime, so ineligible.
  def encrypt(date: LocalDate, key: Array[Byte], tweak: Array[Byte]) = {
    val day = BigInteger.valueOf(date.getDayOfMonth)
    val month = YearMonth.from(date)
    // Use year and month as a deterministic tweak
    val tweakWithMonth = month.toString.getBytes("UTF-8") ++ tweak
    val newDay = walkCycle(day, month, FPE.encrypt(ThirtyTwo, _, key, tweakWithMonth))
    month.atDay(newDay.intValue)
  }
  def decrypt(date: LocalDate, key: Array[Byte], tweak: Array[Byte]) = {
    val day = BigInteger.valueOf(date.getDayOfMonth)
    val month = YearMonth.from(date)
    // Use year and month as a deterministic tweak
    val tweakWithMonth = month.toString.getBytes("UTF-8") ++ tweak
    val newDay = walkCycle(day, month, FPE.decrypt(ThirtyTwo, _, key, tweakWithMonth))
    month.atDay(newDay.intValue)
  }

  private def walkCycle(day: BigInteger, month: YearMonth, action: BigInteger => BigInteger): BigInteger = {
    val result = action(day)
    if (month.isValidDay(result.intValue)) {
      result
    } else {
      walkCycle(result, month, action)
    }
  }
}

/**
 * Encrypt the last three digits of a postcode, to allow coarse grained spatial
 * uses.
 */
object PostcodeSuffixEncryptor extends Encrypter("[0-9][ABD-HJLNP-UW-Z]{2}") {
  override def encrypt(message: String, key: Array[Byte], tweak: Array[Byte]) = {
    val head = message.dropRight(3)
    val tail = message.takeRight(3)
    val tweakedTweak = head.getBytes("UTF-8") ++ tweak
    head + super.encrypt(tail, key, tweakedTweak)
  }

  override def decrypt(message: String, key: Array[Byte], tweak: Array[Byte]) = {
    val head = message.dropRight(3)
    val tail = message.takeRight(3)
    val tweakedTweak = head.getBytes("UTF-8") ++ tweak
    head + super.decrypt(tail, key, tweakedTweak)
  }
}
