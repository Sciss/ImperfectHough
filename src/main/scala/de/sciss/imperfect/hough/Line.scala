package de.sciss.imperfect.hough

//final class Line(var x1: Float, var y1: Float, var x2: Float, var y2: Float)
final class Line(var x1: Int, var y1: Int, var x2: Int, var y2: Int) {
  def copyFrom(that: Line): Unit = {
    this.x1 = that.x1
    this.y1 = that.y1
    this.x2 = that.x2
    this.y2 = that.y2
  }

  def immutable: LineI = new LineI(x1 = x1, y1 = y1, x2 = x2, y2 = y2)

  override def toString: String = s"Line(x1 = $x1, y1 = $y1, x2 = $x2, y2 = $y2)"
}

final class LineI(val x1: Int, val y1: Int, val x2: Int, val y2: Int) {
  override def toString: String = s"LineI(x1 = $x1, y1 = $y1, x2 = $x2, y2 = $y2)"
}