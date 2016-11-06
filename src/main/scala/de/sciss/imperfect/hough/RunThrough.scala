/*
 *  RunThrough.scala
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
import org.bytedeco.javacv.OpenCVFrameConverter

object RunThrough extends App {
  private[this] val toMat = new OpenCVFrameConverter.ToMat

  val fIn     = userHome / "Documents" / "projects" / "Imperfect" / "esc_photos" / "IMG_7773.jpg"
  val matIn   = opencv_imgcodecs.imread(fIn.path)
  val imgIn   = toMat.convert(matIn)
  val scaled  = new Mat
  val width   = 1920
  val height  = 1280 // 1080
  val scx     = width .toDouble / imgIn.imageWidth
  val scy     = height.toDouble / imgIn.imageHeight
  val scale   = math.max(scx, scy)

  opencv_imgproc.resize(/* src = */ matIn, /* dst = */ scaled, /* size = */ new opencv_core.Size(width, height),
    /* fx = */ scale, /* fy = 0.0 */ scale, /* interp = */ opencv_imgproc.INTER_LANCZOS4)

  val sobelH = new Mat
  val sobelV = new Mat
  val canny  = new Mat
  // void cv::Sobel(InputArray src, OutputArray dst, int ddepth, int dx, int dy,
  //                int ksize = 3, double scale = 1, double delta = 0, int borderType = BORDER_DEFAULT
//  opencv_imgproc.Sobel(/* src = */ scaled, /* dst = */ sobelH, /* ddepth = */ opencv_core.CV_32F,
//    /* dx = */ 1, /* dy = */ 0, /* ksize = */ 1, /* scale = */ 1.0,
//    /* delta = */ 0.0, /* borderType = */ opencv_core.BORDER_REPLICATE /* .BORDER_DEFAULT */)
//  opencv_imgproc.Sobel(/* src = */ scaled, /* dst = */ sobelV, /* ddepth = */ opencv_core.CV_32F,
//    /* dx = */ 0, /* dy = */ 1, /* ksize = */ 1, /* scale = */ 1.0,
//    /* delta = */ 0.0, /* borderType = */ opencv_core.BORDER_REPLICATE /* .BORDER_DEFAULT */)
//  opencv_core.addWeighted(sobelH, 1.0, sobelV, 1.0, 0.0, sobel)

  opencv_imgproc.cvtColor(scaled, canny, opencv_imgproc.COLOR_BGR2GRAY)
  opencv_imgproc.Canny(canny, canny, 50.0 /* 40.0 */, 200.0, 3, false)

//  val iter = sobel.createIndexer[FloatRawIndexer]()
//  opencv_core.minMaxLoc()
//  val sz = iter.sizes()
//  assert(sz.toList == List(height, width, 3))
//  println(sz.mkString("[", ", ", "]"))
//  var y = 0
//  while (y < height) {
//    var x = 0
//    while (x < width) {
//      x += 1
//    }
//    y += 1
//  }
//  while (y < height)
//  iter.get()

//  opencv_imgproc.GaussianBlur(???, ???, ???, ???)

  val fOut = userHome / "Documents" / "temp" / "canny.jpg"
  opencv_imgcodecs.imwrite(fOut.path, canny)
}