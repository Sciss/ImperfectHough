package de.sciss.imperfect.hough

import scala.annotation.switch

final class Triangle(var x1: Int, var y1: Int, var x2: Int, var y2: Int, var x3: Int, var y3: Int,
                     var coh: Int = -1) {

  def isCoherent  : Boolean = (coh &  0x80000000) == 0
  def isIncoherent: Boolean = (coh &  0x80000000) != 0
  def mkCoherent  (): Unit  =  coh &= 0x7FFFFFFF
  def mkIncoherent(): Unit  =  coh |= 0x80000000

  def copyFrom(that: Triangle): Unit = {
    this.x1 = that.x1
    this.y1 = that.y1
    this.x2 = that.x2
    this.y2 = that.y2
    this.x3 = that.x3
    this.y3 = that.y3
    this.coh = that.coh
  }

  def x(idx: Int): Int = (idx: @switch) match {
    case 0 => x1
    case 1 => x2
    case 2 => x3
    case _ => throw new IllegalArgumentException(idx.toString)
  }

  def y(idx: Int): Int = (idx: @switch) match {
    case 0 => y1
    case 1 => y2
    case 2 => y3
    case _ => throw new IllegalArgumentException(idx.toString)
  }

  def immutable: TriangleI = new TriangleI(x1 = x1, y1 = y1, x2 = x2, y2 = y2, x3 = x3, y3 = y3, coh = coh)

  override def toString: String = s"Triangle(p1 = ($x1, $y1), p2 = ($x2, $y2), p3 = ($x3, $y3), coh = $coh)"
}


final class TriangleI(val x1: Int, val y1: Int, val x2: Int, val y2: Int, val x3: Int, val y3: Int,
                      val coh: Int) {
  override def toString: String = s"TriangleI(p1 = ($x1, $y1), p2 = ($x2, $y2), p3 = ($x3, $y3), coh = $coh)"

  def isCoherent  : Boolean = (coh &  0x80000000) == 0
  def isIncoherent: Boolean = (coh &  0x80000000) != 0

  def prevIndex: Int = coh & 0xfffffff
  def prevPerm : Int = coh >>> 28   // assuming coherence

  def x(idx: Int): Int = (idx: @switch) match {
    case 0 => x1
    case 1 => x2
    case 2 => x3
    case _ => throw new IllegalArgumentException(idx.toString)
  }

  def y(idx: Int): Int = (idx: @switch) match {
    case 0 => y1
    case 1 => y2
    case 2 => y3
    case _ => throw new IllegalArgumentException(idx.toString)
  }
}