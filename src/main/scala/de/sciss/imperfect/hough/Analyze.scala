/*
 *  Analyze.scala
 *  (Imperfect Reconstruction)
 *
 *  Copyright (c) 2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.imperfect.hough

import java.util

import org.bytedeco.javacpp.opencv_core.Point

import scala.collection.immutable.{IndexedSeq => Vec}

object Analyze {
  case class Config(useProb: Boolean = true, filterSim: Boolean = true, minLineLen: Int = 50, minTriLen: Int = 20,
                    maxLineGap: Int = 20, minAngDeg: Double = 11.25 /* 22.5 */, useTri: Boolean = true,
                    useExtend: Boolean = true)

  def extendLine(ln: Line, width: Int, height: Int): Unit = {
    val dy = ln.y2 - ln.y1
    val dx = ln.x2 - ln.x1
    if (dx == 0) {
      ln.y1 = 0
      ln.y2 = height - 1
    } else if (dy == 0) {
      ln.x1 = 0
      ln.x2 = width - 1
    } else {
      // f(x) = ax + b
      val a   = dy.toDouble / dx
      val b   = ln.y1 - a * ln.x1
      var x0, x1, y0, y1 = 0.0

      if (b < 0) {
        y0 = 0.0
        x0 = (y0 - b) / a
        //          y1 = height - 1
        //          x1 = (y1 - b) / a

      } else if (b >= height) {
        y0 = height - 1
        x0 = (y0 - b) / a
        //          y1 = 0.0
        //          x1 = (y1 - b) / a

      } else {
        x0 = 0.0
        y0 = b
      }
      y1 = ((width - 1) - ln.x1) * a + ln.y1
      if (y1 < 0) {
        y1 = 0.0
        x1 = (y1 - b) / a
      } else if (y1 >= height) {
        y1 = height - 1
        x1 = (y1 - b) / a
      } else {
        x1 = width - 1
      }

      //        val x0i = (x0 + 0.5).toInt
      //        val y0i = (y0 + 0.5).toInt
      //        val x1i = (x1 + 0.5).toInt
      //        val y1i = (y1 + 0.5).toInt

      val x0i = ((ln.x1 + x0) * 0.5 + 0.5).toInt
      val y0i = ((ln.y1 + y0) * 0.5 + 0.5).toInt
      val x1i = ((ln.x2 + x1) * 0.5 + 0.5).toInt
      val y1i = ((ln.y2 + y1) * 0.5 + 0.5).toInt

      ln.x1 = x0i
      ln.y1 = y0i
      ln.x2 = x1i
      ln.y2 = y1i
    }
  }

  def lineIntersects(a: Line, b: Line): Boolean = {
    val opt = intersectLineLine(
      a.x1, a.y1, a.x2, a.y2,
      b.x1, b.y1, b.x2, b.y2)
    opt.isDefined
  }

  def lineIntersection(a: Line, b: Line): Option[Point] = {
    val opt = intersectLineLine(
      a.x1, a.y1, a.x2, a.y2,
      b.x1, b.y1, b.x2, b.y2)
    opt.map { case (xd, yd) =>
      new Point(math.round(xd).toInt, math.round(yd).toInt)
    }
  }

  def angleBetween(ln1: Line, ln2: Line): Double = {
    val dx1 = (ln1.x2 - ln1.x1).toDouble
    val dy1 = (ln1.y2 - ln1.y1).toDouble
    val dx2 = (ln2.x2 - ln2.x1).toDouble
    val dy2 = (ln2.y2 - ln2.y1).toDouble
    val d   =  dx1*dx2 + dy1*dy2   // dot product of the 2 vectors
    val l2  = (dx1*dx1 + dy1*dy1) * (dx2*dx2 + dy2*dy2) // product of the squared lengths
    math.acos(d / math.sqrt(l2))
  }

  def lineLen(ln: Line): Double = {
    val dx = ln.x2 - ln.x1
    val dy = ln.y2 - ln.y1
    math.sqrt(dx * dx + dy * dy)
  }

  def intersectLineLine(a1x: Double, a1y: Double, a2x: Double, a2y: Double,
                        b1x: Double, b1y: Double, b2x: Double, b2y: Double): Option[(Double, Double)] =  {
    val ua_t = (b2x-b1x)*(a1y-b1y)-(b2y-b1y)*(a1x-b1x)
    val ub_t = (a2x-a1x)*(a1y-b1y)-(a2y-a1y)*(a1x-b1x)
    val u_b  = (b2y-b1y)*(a2x-a1x)-(b2x-b1x)*(a2y-a1y)

    if (u_b != 0) {
      val ua = ua_t / u_b
      val ub = ub_t / u_b

      if (0 <= ua && ua <= 1 && 0 <= ub && ub <= 1) {
        val ix = a1x + ua * (a2x - a1x)
        val iy = a1y + ua * (a2y - a1y)
        Some((ix, iy))

      } else {
        None // NO_INTERSECTION
      }
    } else {
      None // if (ua_t == 0 || ub_t == 0) COINCIDENT else PARALLEL
    }
  }

  private[this] object X1Comparator extends Ordering[Line] {
    def compare(a: Line, b: Line): Int = {
      val ax1 = a.x1
      val bx1 = b.x1
      if (ax1 < bx1) -1 else if (ax1 > bx1) 1 else 0
    }
  }

  def removeSimilarLines(lines: Array[Line], numIn: Int, dist: Int = 5): Int = {
    util.Arrays.sort(lines, 0, numIn, X1Comparator)
    val distSq  = dist * dist
    var i       = 0
    var numOut  = 0
    while (i < numIn) {
      val a   = lines(i)
      var sim = false
      var j   = i + 1
      while (j < numIn) {
        val b   = lines(j)
        val dx1 = b.x1 - a.x1
        if (dx1 >= dist) {  // we can short-cut now
          j = numIn
        } else {
          val dy1     = b.y1 - a.y1
          val dx2     = b.x2 - a.x2
          val dy2     = b.y2 - a.y2
          val distSq1 = dx1 * dx1 + dy1 * dy1
          val distSq2 = dx2 * dx2 + dy2 * dy2
          sim         = distSq1 < distSq && distSq2 < distSq
          if (sim) {
            j = numIn
          } else {
            j += 1
          }
        }
      }
      if (!sim) {
        if (i != numOut) {
          val b = lines(numOut)
          b.x1 = a.x1
          b.y1 = a.y1
          b.x2 = a.x2
          b.y2 = a.y2
        }
        numOut += 1
      }
      i += 1
    }
    numOut
  }

  def run(lines: Array[Line], numLines0: Int, width: Int, height: Int, config: Config): Vec[Line] = {
    import config._

    val numLines = if (filterSim) removeSimilarLines(lines, numLines0) else numLines0

    val minAngRad = minAngDeg.toRadians

    if (useExtend) {
      var i = 0
      while (i < numLines) {
        extendLine(lines(i), width = width, height = height)
        i += 1
      }
    }

    if (useTri) {
      val lines1B = lines.take(numLines) /*.filter(lineLen(_) >= minLen) */.toBuffer
      val lines2B = Vector.newBuilder[Line]
      var count1 = 0
      var count2 = 0
      var count3 = 0

      while (lines1B.nonEmpty) {
        val ln1 = lines1B.remove(0)
        var i = 0
        while (i < lines1B.size) {
          val ln2 = lines1B(i)
          val sect1 = lineIntersection(ln1, ln2)
          if (sect1.isDefined && (minAngRad == 0 || angleBetween(ln1, ln2) >= minAngRad)) {
            count1 += 1
//            if (count1 % 1000 == 0) println(s"count1 = $count1")
            var j = i + 1
            while (j < lines1B.size) {
              val ln3 = lines1B(j)
              val sect2 = lineIntersection(ln1, ln3)
              val sect3 = if (sect2.isEmpty) None else lineIntersection(ln2, ln3)
              if (sect2.isDefined && sect3.isDefined &&
                (minAngRad == 0 || (angleBetween(ln1, ln3) >= minAngRad && angleBetween(ln2, ln3) >= minAngRad))) {
                count2 += 1
//                if (count2 % 1000 == 0) println(s"count2 = $count2")

                val p1 = sect1.get
                val p2 = sect2.get
                val p3 = sect3.get

                def rangeCheck(pt: Point): Boolean = {
                  val bad = pt.x < 0 || pt.x > width || pt.y < 0 || pt.y > height
                  if (bad) println(s"out-of-bounds: (${pt.x}, ${pt.y})")
                  bad
                }

                val ln4 = new Line(p1.x, p1.y, p2.x, p2.y)
                val ln5 = new Line(p2.x, p2.y, p3.x, p3.y)
                val ln6 = new Line(p3.x, p3.y, p1.x, p1.y)

                if (rangeCheck(p1) | rangeCheck(p2) | rangeCheck(p3)) {
                  println(s"ln1 = $ln1, ln2 = $ln2, ln3 = $ln3")
                }

                //                lines2B += ln1
                //                lines2B += ln2
                //                lines2B += ln3
                if (minTriLen == 0 || (lineLen(ln4) >= minTriLen && lineLen(ln5) >= minTriLen && lineLen(ln6) >= minTriLen)) {
                  count3 += 1
//                  if (count3 % 1000 == 0) println(s"count3 = $count3")
                  lines2B += ln4
                  lines2B += ln5
                  lines2B += ln6
                  lines1B.remove(j)
                  lines1B.remove(i)
                  j = lines1B.size
                  i = j
                } else {
                  j += 1
                }
              } else {
                j += 1
              }
            }
          }
          i += 1
        }
      }

//      println(s"tri - $count1 / $count2 / $count3")

      lines2B.result()
    } else {
      lines.take(numLines).toVector
    }
  }
}