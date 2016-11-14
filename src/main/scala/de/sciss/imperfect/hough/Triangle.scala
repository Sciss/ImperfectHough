package de.sciss.imperfect.hough

final class Triangle(var x1: Int, var y1: Int, var x2: Int, var y2: Int, var x3: Int, var y3: Int) {
  def copyFrom(that: Triangle): Unit = {
    this.x1 = that.x1
    this.y1 = that.y1
    this.x2 = that.x2
    this.y2 = that.y2
    this.x3 = that.x3
    this.y3 = that.y3
  }

  def immutable: TriangleI = new TriangleI(x1 = x1, y1 = y1, x2 = x2, y2 = y2, x3 = x3, y3 = y3)

  override def toString: String = s"Triangle(p1 = ($x1, $y1), p2 = ($x2, $y2), p3 = ($x3, $y3))"
}


final class TriangleI(val x1: Int, val y1: Int, val x2: Int, val y2: Int, val x3: Int, val y3: Int) {
  override def toString: String = s"TriangleI(p1 = ($x1, $y1), p2 = ($x2, $y2), p3 = ($x3, $y3))"
}