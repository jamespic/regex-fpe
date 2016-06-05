package io.github.jamespic.regex_fpe

import io.github.jamespic.regex_keyspace.RegexAST
import net._95point2.fpe.FPE

/**
  * Created by james on 05/06/16.
  */
class Encrypter(regex: String) {
  val regexAST = RegexAST.parse(regex).get
  def encrypt(message: String, key: Array[Byte], tweak: Array[Byte]) = {
    regexAST.nth(
      FPE.encrypt(
        regexAST.combinations.bigInteger,
        regexAST.indexOf(message).get.bigInteger,
        key,
        tweak
      )
    )
  }
  def decrypt(message: String, key: Array[Byte], tweak: Array[Byte]) = {
    regexAST.nth(
      FPE.decrypt(
        regexAST.combinations.bigInteger,
        regexAST.indexOf(message).get.bigInteger,
        key,
        tweak
      )
    )
  }
}
