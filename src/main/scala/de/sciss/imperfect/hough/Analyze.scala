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

object Analyze {
  case class Config(useProb: Boolean = true, filterSim: Boolean = true, minLineLen: Int = 50, minTriLen: Int = 20,
                    maxLineGap: Int = 20, minAngDeg: Int = 11, useTri: Boolean = true,
                    useExtend: Boolean = false /* true */)

  def extendLine(ln: Line, width: Int, height: Int): Unit = {
    val dy = ln.y2 - ln.y1
    val dx = ln.x2 - ln.x1
    if (dx == 0) {
      // val h = ln.y2 - ln.y1
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

  def angleBetween(a: Line, b: Line): Double = {
    val dax = (a.x2 - a.x1).toDouble
    val day = (a.y2 - a.y1).toDouble
    val dbx = (b.x2 - b.x1).toDouble
    val dby = (b.y2 - b.y1).toDouble
    val d   =  dax*dbx + day*dby   // dot product of the 2 vectors
    val l2  = (dax*dax + day*day) * (dbx*dbx + dby*dby) // product of the squared lengths
    math.acos(d / math.sqrt(l2))
  }

  def lineLen(ln: Line): Double = lineLenPt(ln.x1, ln.y1, ln.x2, ln.y2)

  def lineLenPt(x1: Int, y1: Int, x2: Int, y2: Int): Double = {
    val dx = x2 - x1
    val dy = y2 - y1
    math.sqrt(dx * dx + dy * dy)
  }

  def lineLenPtSq(x1: Int, y1: Int, x2: Int, y2: Int): Int = {
    val dx = x2 - x1
    val dy = y2 - y1
    dx * dx + dy * dy
  }

  def intersectLineLine(a1x: Double, a1y: Double, a2x: Double, a2y: Double,
                        b1x: Double, b1y: Double, b2x: Double, b2y: Double): Option[(Double, Double)] =  {
    val dax   = a2x - a1x
    val day   = a2y - a1y
    val dbx   = b2x - b1x
    val dby   = b2y - b1y
    val dx1   = a1x - b1x
    val dy1   = a1y - b1y
    val ua_t  = dbx*dy1 - dby*dx1
    val ub_t  = dax*dy1 - day*dx1
    val u_b   = dby*dax - dbx*day

    if (u_b != 0) {
      val ua = ua_t / u_b
      val ub = ub_t / u_b

      if (0 <= ua && ua <= 1 && 0 <= ub && ub <= 1) {
        val ix = a1x + ua * dax
        val iy = a1y + ua * day
        Some((ix, iy))

      } else {
        None // NO_INTERSECTION
      }
    } else {
      None // if (ua_t == 0 || ub_t == 0) COINCIDENT else PARALLEL
    }
  }

  private[this] object LineX1Ordering extends Ordering[Line] {
    def compare(a: Line, b: Line): Int = {
      val ax1 = a.x1
      val bx1 = b.x1
      if (ax1 < bx1) -1 else if (ax1 > bx1) 1 else 0
    }
  }

  def removeSimilarLines(lines: Array[Line], numIn: Int, dist: Int = 5): Int = {
    util.Arrays.sort(lines, 0, numIn, LineX1Ordering)
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

  def calcIntersection(a: Line, b: Line, int: Intersection, minAngDeg: Int): Boolean = {
    val dax   = a.x2 - a.x1
    val day   = a.y2 - a.y1
    val dbx   = b.x2 - b.x1
    val dby   = b.y2 - b.y1
    val dx1   = a.x1 - b.x1
    val dy1   = a.y1 - b.y1
    val ua_t  = dbx*dy1 - dby*dx1
    val ub_t  = dax*dy1 - day*dx1
    val u_b   = dby*dax - dbx*day

    val res = (u_b != 0) && {
      val ua = ua_t.toDouble / u_b
      val ub = ub_t.toDouble / u_b

      (0 <= ua && ua <= 1 && 0 <= ub && ub <= 1) && {
        int.x       = (a.x1 + ua * dax + 0.5).toInt
        int.y       = (a.y1 + ua * day + 0.5).toInt

        val d       =  dax*dbx + day*dby   // dot product of the 2 vectors
        val l2      = (dax*dax + day*day).toLong * (dbx*dbx + dby*dby) // product of the squared lengths
        val angRad  = math.acos(d / math.sqrt(l2))
        int.angle   = (angRad * 180 / math.Pi + 0.5).toInt
        int.angle  >= minAngDeg
      }
    }

    if (!res) int.mkInvalid()
    res
  }

  object IntersectionCountOrdering extends Ordering[IntersectionCount] {
    def compare(a: IntersectionCount, b: IntersectionCount): Int = {
      val ac = a.count
      val bc = b.count
      if (ac < bc) -1 else if (ac > bc) 1 else 0
    }
  }
}
final class Analyze(maxLines: Int) {
  import Analyze._

  private[this] val intersections     = Array.fill(maxLines * maxLines)(new Intersection(0, 0, 0))
  private[this] val numIntersectionsS = Array.fill(maxLines)(new IntersectionCount(0, 0))
//  private[this] val numIntersections  = new Array[Int](maxLines)
  private[this] val triTemp           = Array.fill(maxLines)(new Line(0, 0, 0, 0))
  private[this] val triTaken          = new Array[Boolean](maxLines)

  private def calcIntersections(lines: Array[Line], numLines: Int, minAngDeg: Int): Unit = {
    var ai = 0
    while (ai < numLines) {
      val count = numIntersectionsS(ai)
      count.index = ai
      count.count = 0
      ai += 1
    }

    ai = 0
    while (ai < numLines) {
      val a       = lines(ai)
      val aCount  = numIntersectionsS(ai)
      val intIdx0 = ai * maxLines
      var bi      = ai + 1
      while (bi < numLines) {
        val b       = lines(bi)
        val bCount  = numIntersectionsS(bi)
        val intIdxA = intIdx0       + aCount.count
        val intIdxB = bi * maxLines + bCount.count
        val intA    = intersections(intIdxA)
        val intB    = intersections(intIdxB)
        val ok      = calcIntersection(a = a, b = b, int = intA, minAngDeg = minAngDeg)
        if (ok) {
          intB.x          = intA.x
          intB.y          = intA.y
          intB.angle      = intA.angle
          intA.targetIdx  = bi
          intB.targetIdx  = ai
          aCount.count   += 1
          bCount.count   += 1
        }
        bi += 1
      }
      ai += 1
    }

//    ai = 0
//    while (ai < numLines) {
//      numIntersections(ai) = numIntersectionsS(ai).count
//      ai += 1
//    }
    util.Arrays.sort(numIntersectionsS, 0, numLines, IntersectionCountOrdering)
  }

  def run(lines: Array[Line], numLines0: Int, width: Int, height: Int, config: Config): Int = {
    import config._

    val numLines = if (filterSim) removeSimilarLines(lines, numLines0) else numLines0

//    val minAngRad = minAngDeg.toRadians

    if (useExtend) {
      var i = 0
      while (i < numLines) {
        extendLine(lines(i), width = width, height = height)
        i += 1
      }
    }

    if (useTri) {
      calcIntersections(lines, numLines, minAngDeg = minAngDeg)
      util.Arrays.fill(triTaken, 0, numLines, false)
      val minTriLenSq = minTriLen * minTriLen
      var numTriLines = 0
      var p = 0
      while (p < numLines) {
        val count   = numIntersectionsS(p)
        val i       = count.index
        val numInt  = count.count
        if (!triTaken(i) && numInt >= 2) {
          val intIdx0 = i * maxLines
          var intIdx1 = 0
          val numIntM = numInt - 1
          while (intIdx1 < numIntM) {
            val int1 = intersections(intIdx0 + intIdx1)
            assert(int1.isValid)
            val j = int1.targetIdx
            if (!triTaken(j)) {
              var intIdx2 = intIdx1 + 1
              while (intIdx2 < numInt) {
                val int2 = intersections(intIdx0 + intIdx2)
                assert(int2.isValid)
                val k = int2.targetIdx
                if (!triTaken(k)) {
                  val intIdx3 = j * maxLines + k
                  val int3    = intersections(intIdx3)

                  if (int3.isValid && (minTriLen == 0 || (
                      lineLenPtSq(int1.x, int1.y, int2.x, int2.y) >= minTriLenSq &&
                      lineLenPtSq(int2.x, int2.y, int3.x, int3.y) >= minTriLenSq &&
                      lineLenPtSq(int3.x, int3.y, int1.x, int1.y) >= minTriLenSq
                      ))) {

                    val ln1 = triTemp(numTriLines); numTriLines += 1
                    val ln2 = triTemp(numTriLines); numTriLines += 1
                    val ln3 = triTemp(numTriLines); numTriLines += 1
                    ln1.x1  = int1.x
                    ln1.y1  = int1.y
                    ln1.x2  = int2.x
                    ln1.y2  = int2.y
                    ln2.x1  = int2.x
                    ln2.y1  = int2.y
                    ln2.x2  = int3.x
                    ln2.y2  = int3.y
                    ln3.x1  = int3.x
                    ln3.y1  = int3.y
                    ln3.x2  = int1.x
                    ln3.y2  = int1.y

                    triTaken(i) = true
                    triTaken(j) = true
                    triTaken(k) = true

                    // abort loops
                    intIdx2 = numInt
                    intIdx1 = numIntM
                  }
                }
                intIdx2 += 1
              }
            }
            intIdx1 += 1
          }
        }
        p += 1
      }

//      println(s"tri - $count1 / $count2 / $count3")

      p = 0
      while (p < numTriLines) {
        val a = lines  (p)
        val b = triTemp(p)
        a.copyFrom(b)
        p += 1
      }
      numTriLines

    } else {
      numLines
    }
  }
}