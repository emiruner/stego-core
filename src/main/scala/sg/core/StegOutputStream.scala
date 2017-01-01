package sg.core

import java.awt.image.WritableRaster
import java.io.{IOException, OutputStream}

import sg.core.BitUtil.bitAt

class StegOutputStream(image: WritableRaster, bitsToUse: Int) extends OutputStream {
  val engine = new StegEngine(bitsToUse, image)
  var modifiedBitCount = 0

  override def write(data: Int): Unit =
    (0 until 8).foreach(writeBit(data, _))

  def writeBit(data: Int, bitIdx: Int): Unit =
    modifiedBitCount += (if (engine.storeBit(bitAt(data, bitIdx))) 1 else 0)

  def writeInt4(v: Int): Int =
    writeAndShift(writeAndShift(writeAndShift(writeAndShift(v))))

  def writeAndShift(v: Int): Int = {
    write(v & 0xff)
    v >> 8
  }

  def modificationPercentage: Double = modifiedBitCount * 100.0 / engine.imageBitCount

  def fillRemainingWithRandomData(): Unit =
    try {
      while (true) write((Math.random * 256.0).toInt)
    } catch {
      case _: IOException =>
    }
}
