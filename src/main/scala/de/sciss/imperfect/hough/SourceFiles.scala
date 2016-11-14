/*
 *  SourceFiles.scala
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

import akka.actor.Actor
import akka.event.Logging
import de.sciss.file._
import org.bytedeco.javacpp.opencv_core.Mat
import org.bytedeco.javacpp.{opencv_core, opencv_imgcodecs, opencv_imgproc}

final class SourceFiles extends SourceLike with Actor {
  import Source._

  private[this] var imageIdx      = 0
  private[this] val imageIndices  = Array[Int](7763, 7773, 7775, 7777, 7782, 7784, 7787, 7789, 7798, 7854, 7864)
  private[this] val dirIn         = userHome / "Documents" / "projects" / "Imperfect" / "esc_photos"
  private[this] var width     = -1
  private[this] var height    = -1

  def receive: Receive = {
    case Task =>
      val fIn     = dirIn / s"IMG_${imageIndices(imageIdx)}.jpg"
      imageIdx    = (imageIdx + 1) % imageIndices.length
      val matIn   = opencv_imgcodecs.imread(fIn.path)
      val imgIn   = toMat.convert(matIn)
      val scaled  = new Mat
      val scx     = width .toDouble / imgIn.imageWidth
      val scy     = height.toDouble / imgIn.imageHeight
      val scale   = math.max(scx, scy)

      opencv_imgproc.resize(/* src = */ matIn, /* dst = */ scaled, /* size = */ new opencv_core.Size(width, height),
        /* fx = */ scale, /* fy = 0.0 */ scale, /* interp = */ opencv_imgproc.INTER_LANCZOS4)
      val frame = toMat.convert(scaled)
      val res = analyze(frame, dir = 0)
      sender() ! MainLoop.Analysis(res)

    case Open(_width, _height) =>
      log.info("opening")
      width   = _width
      height  = _height

    case Close =>
      log.info("closing")

    case x =>
      log.warning(s"received unknown message '$x'")
  }
}