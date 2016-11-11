/*
 *  LensCorrection.scala
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

object LensCorrection {
  private[this] val baseDir = userHome / "Documents" / "projects" / "Imperfect" / "hough" / "chess"

  // data obtained from
  // ./bin/cpp-example-calibration -w=8 -h=6 -o=/home/hhrutz/Documents/projects/Imperfect/hough/chess/amcrest.yml
  //   -op -oe -su -p -zt /home/hhrutz/Documents/projects/Imperfect/hough/chess/image_list.xml

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

  val cameraMatrix: Mat = {
    val camMat  = new Mat(3, 3, opencv_core.CV_32FC1)
    val camIdx  = camMat.createIndexer[FloatRawIndexer]
    for (row <- 0 until 3) {
      for (col <- 0 until 3) {
        camIdx.put(row, col, cameraMatrixData(row * 3 + col).toFloat)
      }
    }
    camMat
  }

  val distortionVector: Mat = {
    val distVec = new Mat(1, 5, opencv_core.CV_32FC1)
    val distIdx = distVec.createIndexer[FloatRawIndexer]
    for (col <- 0 until 5) {
      distIdx.put(0, col, distMatrixData(col).toFloat)
    }
    distVec
  }

  def correct(in: Mat, out: Mat): Unit =
    opencv_imgproc.undistort(in, out, cameraMatrix, distortionVector)

  def main(args: Array[String]): Unit = {
    val matOut  = new Mat

    for (i <- 1 to 9) {
      val matIn = opencv_imgcodecs.imread((baseDir / s"chess$i.jpg").path)
      correct(matIn, matOut)

      val fOut = userHome / "Documents" / "temp" / s"undistort$i.jpg"
      opencv_imgcodecs.imwrite(fOut.path, matOut)
    }

    for (i <- 2 to 5) {
      val fIn     = userHome / "Documents" / "temp" / "amcrest_test" / s"image$i.jpg"
      val matIn   = opencv_imgcodecs.imread(fIn.path)
      correct(matIn, matOut)

      val fOut = userHome / "Documents" / "temp" / "amcrest_test" / s"image$i-out.jpg"
      opencv_imgcodecs.imwrite(fOut.path, matOut)
    }
  }
}
