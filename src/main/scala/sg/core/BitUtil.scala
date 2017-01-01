package sg.core

object BitUtil {
  def bitAt(v: Int, idx: Int): Int = (v >> idx) & 1
}
