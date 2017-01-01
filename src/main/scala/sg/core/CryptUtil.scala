package sg.core

import java.security.SecureRandom
import javax.crypto.spec.{IvParameterSpec, PBEKeySpec, SecretKeySpec}
import javax.crypto.{Cipher, SecretKeyFactory}

object CryptUtil {
  def randomBytes(count: Int): Array[Byte] = {
    val sr = new SecureRandom()
    val bytes = new Array[Byte](count)

    sr.nextBytes(bytes)
    bytes
  }

  val salt: Array[Byte] = Array(0xc7, 0x73, 0x21, 0x8c, 0x7e, 0xc8, 0xee, 0x99).map(_.toByte)

  def createSimpleCipher(passphrase: String, iv: Array[Byte], mode: Int): Cipher = {
    val factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1")
    val keySpec = new PBEKeySpec(passphrase.toCharArray, salt, 65536, 128)
    val secret = new SecretKeySpec(factory.generateSecret(keySpec).getEncoded, "AES")

    val pbeCipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    pbeCipher.init(mode, secret, new IvParameterSpec(iv))

    pbeCipher
  }
}
