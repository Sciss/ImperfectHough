/*
 *  LensCorrectionTest.scala
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

import de.sciss.file._
import org.bytedeco.javacpp.indexer.FloatRawIndexer
import org.bytedeco.javacpp.opencv_core.Mat
import org.bytedeco.javacpp.{opencv_core, opencv_imgcodecs, opencv_imgproc}

object LensCorrectionTest {
  val baseDir = userHome / "Documents" / "projects" / "Imperfect" / "hough" / "chess"
  val cameraFocal = 1.4656877976320607e+03
  val cameraCX    = 1920.0/2
  val cameraCY    = 1080.0/2

  val cameraMatrixData = Array[Double](
    cameraFocal, 0.0        , cameraCX,
    0.0        , cameraFocal, cameraCY,
    0.0        , 0.0        , 1.0
  )

  val distMatrixData = Array[Double](
    -4.0168243817428867e-01, 4.3688424930745831e-02, 0.0, 0.0, 1.0964121427048183e-01
  )

  def main(args: Array[String]): Unit = {
    val matOut  = new Mat
    val camMat  = new Mat(3, 3, opencv_core.CV_32FC1)
    val camIdx  = camMat.createIndexer[FloatRawIndexer]
    for (row <- 0 until 3) {
      for (col <- 0 until 3) {
        camIdx.put(row, col, cameraMatrixData(row * 3 + col).toFloat)
      }
    }
    val distVec = new Mat(1, 5, opencv_core.CV_32FC1)
    val distIdx = distVec.createIndexer[FloatRawIndexer]
    for (col <- 0 until 5) {
      distIdx.put(0, col, distMatrixData(col).toFloat)
    }

    for (i <- 1 to 9) {
      val matIn   = opencv_imgcodecs.imread((baseDir / s"chess$i.jpg").path)
      opencv_imgproc.undistort(matIn, matOut, camMat, distVec)

      val fOut = userHome / "Documents" / "temp" / s"undistort$i.jpg"
      opencv_imgcodecs.imwrite(fOut.path, matOut)
    }
  }
}
