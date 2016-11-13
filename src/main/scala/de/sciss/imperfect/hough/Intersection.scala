package de.sciss.imperfect.hough

final class Intersection(var x: Int, var y: Int, var angle: Int, var targetIdx: Int = -1) {
  def isValid: Boolean  = targetIdx >= 0

  def mkInvalid(): Unit = targetIdx = -1

  override def toString: String =
    if (isValid)
      s"Intersection(x = $x, y = $y, angle = $angle, targetIdx = $targetIdx)"
    else
      "Intersection(<invalid>)"
}