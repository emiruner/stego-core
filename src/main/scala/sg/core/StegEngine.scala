package sg.core

import java.awt.image.{Raster, WritableRaster}
import java.io.IOException

import BitUtil._

case class ImageDataCursor(bitsToUse: Int, raster: WritableRaster, bitIdx: Int = 0, byteIdx: Int = 0, x: Int = 0, y: Int = 0) {
  def nextBit: Option[ImageDataCursor] = {
    if (bitIdx + 1 == bitsToUse) {
      nextByte
    } else {
      Some(this.copy(bitIdx = bitIdx + 1))
    }
  }

  private def nextByte = {
    if (byteIdx + 1 == raster.getNumDataElements) {
      nextColumn
    } else {
      Some(this.copy(bitIdx = 0, byteIdx = byteIdx + 1))
    }
  }

  private def nextColumn = {
    if (x + 1 == raster.getWidth) {
      nextRow
    } else {
      Some(this.copy(bitIdx = 0, byteIdx = 0, x = x + 1))
    }
  }

  private def nextRow = {
    if (y + 1 == raster.getHeight) {
      None
    } else {
      Some(this.copy(bitIdx = 0, byteIdx = 0, x = 0, y = y + 1))
    }
  }

  def storeBit(bitValue: Int): Boolean = {
    val rgb = new Array[Byte](raster.getNumDataElements)
    raster.getDataElements(x, y, rgb)

    val targetBitDigit = 1 << bitIdx
    val targetByte = rgb(byteIdx)
    val targetBitOldValue = bitAt(targetByte, bitIdx)

    rgb(byteIdx) = (if (bitValue == 1) targetByte | targetBitDigit else targetByte & ~targetBitDigit).toByte

    raster.setDataElements(x, y, rgb)

    bitValue != targetBitOldValue
  }

  def readBit: Int = {
    val rgb = new Array[Byte](raster.getNumDataElements)
    raster.getDataElements(x, y, rgb)
    bitAt(rgb(byteIdx), bitIdx)
  }
}

class StegEngine(bitsToUse: Int, raster: WritableRaster) {
  assert((1 to 7) contains bitsToUse)

  private var maybeCursor = ImageDataCursor(bitsToUse, raster, -1).nextBit

  def storeBit(bit: Int): Boolean = maybeCursor match {
    case Some(cursor) =>
      val changed = cursor.storeBit(bit)
      maybeCursor = cursor.nextBit
      changed

    case None =>
      throw new IOException("Image capacity exceeded.")
  }

  def readBit: Int = maybeCursor match {
    case Some(cursor) =>
      val bit = cursor.readBit
      maybeCursor = cursor.nextBit
      bit

    case None =>
      throw new IOException("Image capacity exceeded.")
  }

  def imageBitCount: Int = StegEngine.imageByteCount(raster) * 8
}

object StegEngine {
  def calculateImageBitCapacity(raster: Raster, bitsToUse: Int): Int = imageByteCount(raster) * bitsToUse / 8

  def imageByteCount(raster: Raster): Int = raster.getWidth * raster.getHeight * raster.getNumDataElements
}
