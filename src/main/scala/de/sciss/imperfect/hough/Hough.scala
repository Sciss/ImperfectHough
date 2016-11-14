/*
 *  Hough.scala
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

import org.bytedeco.javacpp.indexer.IntRawIndexer
import org.bytedeco.javacpp.opencv_core.Mat
import org.bytedeco.javacpp.opencv_imgproc

final class Hough(maxLines: Int) {
//  private[this] val minLineLen  = 50
//  private[this] val maxLineGap  = 20
  private[this] val linesMat    = new Mat()
  private[this] val rho         = 1.0
  private[this] val theta       = 1.0.toRadians

  def run(matIn: Mat, lines: Array[Line], minLineLen: Int = 50, maxLineGap: Int = 20, thresh: Int = 50,
          force: Boolean = false): Int = {
    opencv_imgproc.HoughLinesP(matIn, linesMat, rho, theta, thresh, minLineLen, maxLineGap)

    val indexer: IntRawIndexer = try {
      linesMat.createIndexer()
    } catch {
      case _: NullPointerException => return 0 // WTF -- happens occasionally
    }

    val numLinesL = indexer.rows()
    if (!force && numLinesL > maxLines) return math.min(0x7FFFFFFF, numLinesL).toInt
    val numLines  = math.min(maxLines, numLinesL).toInt
    //    println(s"numLines = $numLines")

    var i = 0
    while (i < numLines) {
      val x1  = indexer.get(i, 0, 0)
      val y1  = indexer.get(i, 0, 1)
      val x2  = indexer.get(i, 0, 2)
      val y2  = indexer.get(i, 0, 3)

      val ln  = lines(i)
      if (x1 < x2 || (x1 == x2 && y1 < y2)) {
        ln.x1   = x1
        ln.y1   = y1
        ln.x2   = x2
        ln.y2   = y2
      } else {
        ln.x1   = x2
        ln.y1   = y2
        ln.x2   = x1
        ln.y2   = y1
      }
      i += 1
    }

    numLines
  }
}
