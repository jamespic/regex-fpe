package io.github.jamespic.regex_fpe

/**
  * Created by james on 05/06/16.
  */
object EncrypterBenchmark extends App {
  val key = "secret key".getBytes("UTF-8")
  val tweak = "tweak".getBytes("UTF-8")
  val encrypter = new Encrypter("\\d{9}")

  var lastReport = System.nanoTime()
  var lastResult: String = _
  for (i <- 100000000 to 999999999) {
    lastResult = encrypter.decrypt(encrypter.encrypt(i.toString, key, tweak), key, tweak)
    if (i % 1000 == 0) {
      val newLastReport = System.nanoTime
      println(s"encrypting at a rate of ${1000.0 * 1000000000.0 / (newLastReport - lastReport)} per second")
      lastReport = newLastReport
    }
  }

}
