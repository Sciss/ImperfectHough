package de.sciss.imperfect.hough

import scala.annotation.switch

final class Velocities(var x1: Float, var vx1: Float, var y1: Float, var vy1: Float,
                       var x2: Float, var vx2: Float, var y2: Float, var vy2: Float,
                       var x3: Float, var vx3: Float, var y3: Float, var vy3: Float) {

  def copyFrom(that: Velocities): Unit = {
    this.x1   = that.x1
    this.vx1  = that.vx1
    this.y1   = that.y1
    this.vy1  = that.vy1
    this.x2   = that.x2
    this.vx2  = that.vx2
    this.y2   = that.y2
    this.vy2  = that.vy2
    this.x3   = that.x3
    this.vx3  = that.vx3
    this.y3   = that.y3
    this.vy3  = that.vy3
  }

  def copyFromPerm(that: Velocities, perm: TriPerm): Unit = {
    this.x1   = that.x (perm._1)
    this.vx1  = that.vx(perm._1)
    this.y1   = that.y (perm._1)
    this.vy1  = that.vy(perm._1)
    this.x2   = that.x (perm._2)
    this.vx2  = that.vx(perm._2)
    this.y2   = that.y (perm._2)
    this.vy2  = that.vy(perm._2)
    this.x3   = that.x (perm._3)
    this.vx3  = that.vx(perm._3)
    this.y3   = that.y (perm._3)
    this.vy3  = that.vy(perm._3)
  }

  def copyFrom(that: TriangleI): Unit = {
    this.x1   = that.x1
    this.vx1  = 0
    this.y1   = that.y1
    this.vy1  = 0
    this.x2   = that.x2
    this.vx2  = 0
    this.y2   = that.y2
    this.vy2  = 0
    this.x3   = that.x3
    this.vx3  = 0
    this.y3   = that.y3
    this.vy3  = 0
  }

  def x(idx: Int): Float = (idx: @switch) match {
    case 0 => x1
    case 1 => x2
    case 2 => x3
    case _ => throw new IllegalArgumentException(idx.toString)
  }

  def y(idx: Int): Float = (idx: @switch) match {
    case 0 => y1
    case 1 => y2
    case 2 => y3
    case _ => throw new IllegalArgumentException(idx.toString)
  }

  def vx(idx: Int): Float = (idx: @switch) match {
    case 0 => vx1
    case 1 => vx2
    case 2 => vx3
    case _ => throw new IllegalArgumentException(idx.toString)
  }

  def vy(idx: Int): Float = (idx: @switch) match {
    case 0 => vy1
    case 1 => vy2
    case 2 => vy3
    case _ => throw new IllegalArgumentException(idx.toString)
  }
}