/*
 *  SourceIPCam.scala
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

import java.io.ByteArrayOutputStream

import akka.actor.Actor
import akka.event.Logging
import org.bytedeco.javacpp.indexer.UByteRawIndexer
import org.bytedeco.javacpp.opencv_core.Mat
import org.bytedeco.javacpp.{opencv_core, opencv_imgcodecs, opencv_imgproc}
import org.bytedeco.javacv.Frame

import scala.sys.process.{Process, ProcessBuilder}

final class SourceIPCam(ip: String, password: String, hAngleStep: Double, vAngle: Double)
  extends SourceLike with Actor {

  import Source._

  private[this] val log = Logging(context.system, this)

  private[this] val cmdCapture = Seq("wget", s"http://$ip/cgi-bin/snapshot.cgi",
    "--user", "admin", "--password", password, "-O-", "-q")

  // init capacity of 640K should be fine for basically all images
  private[this] val osCapture = new ByteArrayOutputStream(640 * 1024)
  private[this] val pbCapture: ProcessBuilder = {
    import sys.process._
    Process(cmdCapture).#>(osCapture)
  }

  private[this] var width     = -1
  private[this] var height    = -1
  private[this] val matCorr   = new Mat
  private[this] val scaled    = new Mat
  private[this] var hAngle    = 0.0
  private[this] var hAngleDir = 1

  private def cmdPTZ(): Seq[String] = {
    val url = s"http://$ip/cgi-bin/ptz.cgi?action=start&channel=0&code=PositionABS&arg1=$hAngle&arg2=$vAngle&arg3=0&arg4=0"
    Seq("wget", url, "--user", "admin", "--password", password, "-O-", "-q")
  }

  private def performPTZ(inc: Boolean = true): Unit = {
    osCapture.reset()
    val resPTZ = Process(cmdPTZ()).#>(osCapture).!
    require(resPTZ == 0, s"'wget' for pan/tilt/zoom exited with error code $resPTZ")
    if (inc) hAngle += hAngleStep * hAngleDir
    if (hAngle < 0 || hAngle > 360) {
      hAngle    = -hAngle
      hAngleDir = -hAngleDir
    }
  }

  private def capture(): Mat = {
    osCapture.reset()
    val resCap  = pbCapture.!
    require(resCap == 0, s"'wget' for capture exited with error code $resCap")
    val arr     = osCapture.toByteArray
    val sz      = arr.length

    // we adjust PTZ _after_ the capture, that way we don't get blurry image
    performPTZ()

    val matRaw  = new Mat(1, sz, opencv_core.CV_8UC1)
    val idxRaw  = matRaw.createIndexer[UByteRawIndexer]()
    var i = 0
    while (i < sz) {
      // XXX TODO --- this can't be the most efficient way???
      idxRaw.put(0, i, arr(i))
      i += 1
    }
    opencv_imgcodecs.imdecode(matRaw, opencv_imgcodecs.IMREAD_COLOR)
  }

  def receive: Receive = {
    case Task =>
      val matIn   = capture()
      LensCorrection.correct(matIn, matCorr)
      log.info("corrected")
      val imgIn   = toMat.convert(matCorr)
      val imgSc: Frame = if (imgIn.imageWidth == width && imgIn.imageHeight == height) imgIn else {
        val scx    = width.toDouble  / imgIn.imageWidth
        val scy    = height.toDouble / imgIn.imageHeight
        val scale  = math.max(scx, scy)

        opencv_imgproc.resize(/* src = */ matCorr, /* dst = */ scaled, /* size = */ new opencv_core.Size(width, height),
          /* fx = */ scale, /* fy = 0.0 */ scale, /* interp = */ opencv_imgproc.INTER_LANCZOS4)
        toMat.convert(scaled)
      }
      val res = analyze(imgSc)
      log.info(s"analyzed ${res.size}")
      sender() ! MainLoop.Analysis(res)

    case Open(_width, _height) =>
      log.info("opening")
      width   = _width
      height  = _height
      performPTZ(inc = false) // initial position

    case Close =>
      log.info("closing")

    case x =>
      log.warning(s"received unknown message '$x'")
  }
}