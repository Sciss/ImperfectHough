/*
 *  SourceLive.scala
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
import org.bytedeco.javacpp.opencv_core
import org.bytedeco.javacv.FrameGrabber.ImageMode
import org.bytedeco.javacv.OpenCVFrameGrabber

final class SourceLive extends SourceLike with Actor {
  import Source._

  private[this] val grabber = new OpenCVFrameGrabber(0)

  def receive: Receive = {
    case Task =>
      // grabber.trigger()
      val frame = grabber.grab()
      val res = analyze(frame)
      sender() ! MainLoop.Analysis(res)

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
      log.info(s"capture width = ${grabber.getImageWidth}, height = ${grabber.getImageHeight}")

    case Close =>
      log.info("closing")
      grabber.stop()

    case x =>
      log.warning(s"received unknown message '$x'")
  }
}