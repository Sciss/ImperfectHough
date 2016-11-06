/*
 *  Source.scala
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
import org.bytedeco.javacpp.opencv_core.Mat
import org.bytedeco.javacpp.{opencv_core, opencv_imgproc}
import org.bytedeco.javacv.FrameGrabber.ImageMode
import org.bytedeco.javacv.{OpenCVFrameConverter, OpenCVFrameGrabber}

object Source {
  final case class Open(width: Int, height: Int)
  case object Task
  case object Close

}
final class Source extends Actor {
  import Source._

  private[this] val log     = Logging(context.system, this)
  private[this] val grabber = new OpenCVFrameGrabber(0)
  private[this] val toMat   = new OpenCVFrameConverter.ToMat

  def receive: Receive = {
    case Task =>
      // grabber.trigger()
      val frame = grabber.grab()
      val matIn = toMat.convert(frame)
      val sobel = new Mat
      // void cv::Sobel(InputArray src, OutputArray dst, int ddepth, int dx, int dy,
      //                int ksize = 3, double scale = 1, double delta = 0, int borderType = BORDER_DEFAULT
      opencv_imgproc.Sobel(/* src = */ matIn, /* dst = */ sobel, /* ddepth = */ opencv_core.CV_32F,
        /* dx = */ 1, /* dy = */ 1, /* ksize = */ 3, /* scale = */ 1.0,
        /* delta = */ 0.0, /* borderType = */ opencv_core.BORDER_DEFAULT)
      opencv_imgproc.GaussianBlur(???, ???, ???, ???)

    case Open(width, height) =>
      log.info("opening")
      grabber.setImageWidth (width )
      grabber.setImageHeight(height)
      grabber.setBitsPerPixel(opencv_core.CV_8U)
      grabber.setImageMode(ImageMode.COLOR)
      // grabber.setFrameRate()
      // grabber.setGamma()
      // grabber.setNumBuffers()
      // grabber.setTriggerMode(true)
      grabber.start()

    case Close =>
      log.info("closing")
      grabber.stop()

    case x =>
      log.warning(s"received unknown message '$x'")
  }
}