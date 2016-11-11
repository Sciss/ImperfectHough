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

import org.bytedeco.javacpp.indexer.{FloatRawIndexer, IntRawIndexer}
import org.bytedeco.javacpp.opencv_core.{Mat, Point}
import org.bytedeco.javacpp.opencv_imgproc

import scala.collection.immutable.{IndexedSeq => Vec}

object Analyze {
  case class Config(useProb: Boolean = true, filterSim: Boolean = true, minLineLen: Int = 50, minTriLen: Int = 20,
                    maxLineGap: Int = 20, minAngDeg: Double = 11.25 /* 22.5 */, useTri: Boolean = true,
                    useExtend: Boolean = true)

  case class Line(pt1: Point, pt2: Point) {
    override def toString = s"Line(${pt1.x}, ${pt1.y}, ${pt2.x}, ${pt2.y})"

    def intersects(that: Line): Boolean = {
      val opt = intersectLineLine(
        this.pt1.x, this.pt1.y, this.pt2.x, this.pt2.y,
        that.pt1.x, that.pt1.y, that.pt2.x, that.pt2.y)
      opt.isDefined
    }

    def intersection(that: Line): Option[Point] = {
      val opt = intersectLineLine(
        this.pt1.x, this.pt1.y, this.pt2.x, this.pt2.y,
        that.pt1.x, that.pt1.y, that.pt2.x, that.pt2.y)
      opt.map { case (xd, yd) =>
        new Point(math.round(xd).toInt, math.round(yd).toInt)
      }
    }

    def extend(width: Int, height: Int): Line = {
      val dy = pt2.y - pt1.y
      val dx = pt2.x - pt1.x
      if (dx == 0) {
        Line(new Point(pt1.x, 0), new Point(pt1.x, height - 1))
      } else if (dy == 0) {
        Line(new Point(0, pt1.y), new Point(width - 1, pt1.y))
      } else {
        // f(x) = ax + b
        val a   = dy.toDouble / dx
        val b   = pt1.y - a * pt1.x
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
        y1 = ((width - 1) - pt1.x) * a + pt1.y
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

        val x0i = ((pt1.x + x0) * 0.5 + 0.5).toInt
        val y0i = ((pt1.y + y0) * 0.5 + 0.5).toInt
        val x1i = ((pt2.x + x1) * 0.5 + 0.5).toInt
        val y1i = ((pt2.y + y1) * 0.5 + 0.5).toInt

        Line(new Point(x0i, y0i), new Point(x1i,y1i))
      }
    }

    //    def intersection(that: Line, width: Int, height: Int): Option[Point] = {
    //      val thisExt = this.extend(width, height)
    //      val thatExt = that.extend(width, height)
    //
    //      val opt = intersectLineLine(
    //        thisExt.pt1.x, thisExt.pt1.y, thisExt.pt2.x, thisExt.pt2.y,
    //        thatExt.pt1.x, thatExt.pt1.y, thatExt.pt2.x, thatExt.pt2.y)
    //      opt.map { case (xd, yd) =>
    //        new Point(math.round(xd).toInt, math.round(yd).toInt)
    //      }
    //    }
  }

  def angleBetween(ln1: Line, ln2: Line): Double = {
    val dx1 = (ln1.pt2.x - ln1.pt1.x).toDouble
    val dy1 = (ln1.pt2.y - ln1.pt1.y).toDouble
    val dx2 = (ln2.pt2.x - ln2.pt1.x).toDouble
    val dy2 = (ln2.pt2.y - ln2.pt1.y).toDouble
    val d   =  dx1*dx2 + dy1*dy2   // dot product of the 2 vectors
    val l2  = (dx1*dx1 + dy1*dy1) * (dx2*dx2 + dy2*dy2) // product of the squared lengths
    math.acos(d / math.sqrt(l2))
  }

  def lineLen(ln: Line): Double = {
    val dx = ln.pt2.x - ln.pt1.x
    val dy = ln.pt2.y - ln.pt1.y
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

  def run(matIn: Mat, config: Config): Vec[Line] = {
    import config._
    val mat         = matIn
    val greyMat     = matIn // new Mat()
    val lines       = new Mat()
//    opencv_imgproc.cvtColor(mat, greyMat, opencv_imgproc.CV_BGR2GRAY, 1)
    val rho         = 1.0
    val theta       = 1.0.toRadians
    val thresh      = 50
    //    val minLineLen  = 40
    //    val maxLineGap  = 20 // 10

    //    val colrRed     = new Scalar(0, 0, 255, 128)
    //    val colrBlack   = new Scalar(0, 0, 0, 255)

    def houghP(): Vec[Line] = {
      opencv_imgproc.HoughLinesP(greyMat, lines, rho, theta, thresh, minLineLen, maxLineGap)

      //      println(s"rows = ${lines.rows()}; cols = ${lines.cols()}")
      val indexer: IntRawIndexer = try {
        lines.createIndexer() //.asInstanceOf[UByteRawIndexer]
      } catch {
        case _: NullPointerException => return Vector.empty // WTF -- happens occasionally
      }
      //      println(s"sizes = ${indexer.sizes().mkString("[", ", ", "]")}")

      val lines0 = for (i <- 0 until indexer.rows().toInt /* lines.rows() */ ) yield {
        val x1 = indexer.get(i, 0, 0)
        val y1 = indexer.get(i, 0, 1)
        val x2 = indexer.get(i, 0, 2)
        val y2 = indexer.get(i, 0, 3)

        //        println(s"x1 = $x1, y1 = $y1, x2 = $x2, y2 = $y2")

        val pt1 = new Point(x1, y1)
        val pt2 = new Point(x2, y2)
        Line(pt1, pt2)
      }
      lines0
    }

    def hough(): Vec[Line] = {
      opencv_imgproc.HoughLines(greyMat, lines, rho, theta, thresh)

      //      println(s"rows = ${lines.rows()}; cols = ${lines.cols()}")
      val indexer: FloatRawIndexer = lines.createIndexer() //.asInstanceOf[UByteRawIndexer]
      //      println(s"sizes = ${indexer.sizes().mkString("[", ", ", "]")}")

      val lines0 = for (i <- 0 until indexer.rows().toInt /* lines.rows() */) yield {
        val rhoV    = indexer.get(i, 0, 0)
        val thetaV  = indexer.get(i, 0, 1)
        //        println(s"rho = $rhoV, theta = $thetaV")

        val a   = math.cos(thetaV)
        val b   = math.sin(thetaV)
        val x0  = a * rhoV
        val y0  = b * rhoV
        val x1  = math.round(x0 + 100 * (-b)).toInt
        val y1  = math.round(y0 + 100 * a).toInt
        val x2  = math.round(x0 - 100 * (-b)).toInt
        val y2  = math.round(y0 - 100 * a).toInt
        val pt1 = new Point(x1, y1)
        val pt2 = new Point(x2, y2)

        Line(pt1, pt2)
      }
      lines0
    }

//    val t1 = System.currentTimeMillis()
    val lines0 = if (useProb) houghP() else hough()
//    val t2 = System.currentTimeMillis()
//    println(s"Analysis took ${t2-t1}ms.")

//    println(s"[0] ${lines0.size}")

    val lines1 = if (!filterSim) lines0 else lines0.tails.flatMap {
      case head +: tail =>
        val isDup = tail.exists { that =>
          val dx1 = head.pt1.x - that.pt1.x
          val dy1 = head.pt1.y - that.pt1.y
          val dx2 = head.pt2.x - that.pt2.x
          val dy2 = head.pt2.y - that.pt2.y
          val distSq1 = dx1 * dx1 + dy1 * dy1
          val distSq2 = dx2 * dx2 + dy2 * dy2
          distSq1 < 20 && distSq2 < 20
        }
        if (isDup) None else Some(head)

      case _ => None
    }   .toVector

//    println(s"[1] ${lines1.size}")

    val minAngRad = minAngDeg.toRadians

    val width   = mat.cols
    val height  = mat.rows

    val lines2 = if (useExtend) lines1.map { ln =>
      val res = ln.extend(width, height)
      // if (res.pt1.x > width || res.pt2.x > width || res.pt1.y > height || res.pt2.y > height) {
      //  println(s"RANGE $ln -> $res")
      // }
      res
    } else lines1

    val lines3 = if (!useTri) lines2 else {
      val lines1B = lines2 /*.filter(lineLen(_) >= minLen) */.toBuffer
      val lines2B = Vector.newBuilder[Line]
      var count1 = 0
      var count2 = 0
      var count3 = 0

      while (lines1B.nonEmpty) {
        val ln1 = lines1B.remove(0)
        var i = 0
        while (i < lines1B.size) {
          val ln2 = lines1B(i)
          val sect1 = ln1.intersection(ln2)
          if (sect1.isDefined && (minAngRad == 0 || angleBetween(ln1, ln2) >= minAngRad)) {
            count1 += 1
//            if (count1 % 1000 == 0) println(s"count1 = $count1")
            var j = i + 1
            while (j < lines1B.size) {
              val ln3 = lines1B(j)
              val sect2 = ln1.intersection(ln3)
              val sect3 = if (sect2.isEmpty) None else ln2.intersection(ln3)
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

                val ln4 = Line(p1, p2)
                val ln5 = Line(p2, p3)
                val ln6 = Line(p3, p1)

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
    }

//    println(s"[2] ${lines3.size}")

    //      val lines2 = testTriangle(lines1, 0, Vector.empty)
    //      val lines2 = lines1

    //      val numTri = lines1.tails.count {
    //        case head +: tail => tail.count(head.intersects) > 2
    //        case _ => false
    //      }
    //      println(s"numTri = $numTri")

    //    val black = new Scalar(0, 0, 0, 0)
    //    mat.setTo(black)

    //    val rect    = new Rect(0, 0, width, height)
//    println(s"w = $width, h = $height")

    // bloody ting throws assertion error
    //    opencv_imgproc.rectangle(mat, rect, colrBlack, 1, opencv_imgproc.CV_FILLED, 0)

    // diese C scheisse ist wird mir jetzt echt zu bloed,
    // muss man zwei jahre studieren, um rauszufinden,
    // wie man die dusselige matrix auf null setzt

//    val out = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
//    val g = out.createGraphics()
//    val t3 = System.currentTimeMillis()
//    g.setColor(Color.black)
//    g.fillRect(0, 0, width, height)
//    g.setColor(Color.white)
//    g.setStroke(new BasicStroke(2f))
//    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
//    lines3.foreach { case Line(pt1, pt2) =>
//      g.drawLine(pt1.x, pt1.y, pt2.x, pt2.y)
//    }
//    g.dispose()
//    val t4 = System.currentTimeMillis()
////    println(s"Line drawing took ${t4-t3}ms")
//    ImageIO.write(out, "jpeg", fOut)
//    out.flush()

    lines3

    //    lines2.foreach { case Line(pt1, pt2) =>
    //      opencv_imgproc.line(mat, pt1, pt2, colrRed, 1, opencv_core.LINE_AA, 0)
    //    }
    //
    //    val pathOut = fOut.path
    //    opencv_imgcodecs.imwrite(pathOut, mat)
  }
}