package sg.core

import java.awt.image.WritableRaster
import java.io.InputStream

class StegInputStream(encodedImage: WritableRaster, bitsToUse: Int) extends InputStream {
  val engine = new StegEngine(bitsToUse, encodedImage)

  override def read: Int = readAndScale(1, 0)

  def readAndScale(digitValue: Int, total: Int): Int =
    if(digitValue == 256) total else readAndScale(digitValue << 1, total + engine.readBit * digitValue)

  def readInt4: Int = read + (read << 8) + (read << 16) + (read << 24)
}
