package sg.core

import java.awt.image.WritableRaster
import java.io.{ByteArrayOutputStream, IOException}
import javax.crypto.{Cipher, CipherOutputStream}
import sg.core.CryptUtil._

object CryptStego {
  def embedData(hostImage: WritableRaster, guestData: Array[Byte], usedBits: Int, passphrase: String): Double = {
    val stegOutput = new StegOutputStream(hostImage, usedBits)
    val iv = CryptUtil.randomBytes(16)

    stegOutput.writeInt4(guestData.length)
    stegOutput.write(iv)

    val output = new CipherOutputStream(stegOutput, createSimpleCipher(passphrase, iv, Cipher.ENCRYPT_MODE))

    output.write(guestData)
    output.flush()

    // Fill rest of the image with random data.
    try {
      while (true) output.write((Math.random * 256.0).toInt)
    } catch {
      case _: IOException =>
    }

    stegOutput.modificationPercentage
  }

  def extractData(hostImage: WritableRaster, usedBits: Int, passphrase: String): Array[Byte] = {
    val output = new ByteArrayOutputStream
    val sis = new StegInputStream(hostImage, usedBits)
    val dataFileSize = sis.readInt4

    val iv = new Array[Byte](16)
    sis.read(iv)

    val cipher = createSimpleCipher(passphrase, iv, Cipher.DECRYPT_MODE)
    val buf = new Array[Byte](16)

    var writtenByteCount = 0
    while (writtenByteCount < dataFileSize) {
      val readCount = sis.read(buf)
      val decrypted = cipher.update(buf, 0, readCount)

      if ((writtenByteCount + decrypted.length) > dataFileSize) {
        output.write(decrypted, 0, dataFileSize - writtenByteCount)
      } else {
        output.write(decrypted)
      }

      writtenByteCount += decrypted.length
    }

    output.flush()
    output.toByteArray
  }
}
