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

import java.awt.image.BufferedImage

import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import org.bytedeco.javacpp.indexer.UByteRawIndexer
import org.bytedeco.javacpp.opencv_core.{Mat, Size}
import org.bytedeco.javacpp.{opencv_core, opencv_imgproc}
import org.bytedeco.javacv.{Frame, Java2DFrameConverter, OpenCVFrameConverter}

import scala.annotation.tailrec

object Source {
  final case class Open(width: Int, height: Int)
  case object Task
  case object Close
  final case class Control(flags: Int)

  final case class GrayImage  (img: BufferedImage)
  final case class ThreshImage(img: BufferedImage)

  def live (): Props = Props(new SourceLive)
  def files(): Props = Props(new SourceFiles)
  def ipCam(ip: String, password: String, hAngleStep: Double, vAngle: Double): Props =
    Props(new SourceIPCam(ip = ip, password = password, hAngleStep = hAngleStep, vAngle = vAngle))


  final val CtlNone   = 0x00
  final val CtlGray   = 0x01
  final val CtlThresh = 0x02
}
abstract class SourceLike extends Actor {
  import Source.{CtlGray, CtlNone, CtlThresh, GrayImage, ThreshImage}

  protected final val toMat       = new OpenCVFrameConverter.ToMat
  protected final val toJava2D    = new Java2DFrameConverter
  private[this]   val anaCfg      = Analyze.Config()
  private[this]   val edge        = new Mat
//  private[this]   val blur      = new Mat
  private[this]   val gray        = new Mat
  private[this]   val blackWhite  = new Mat
  private[this]   var ctl         = Option.empty[ActorRef]
  private[this]   var ctlFlags    = CtlNone: Int

  protected final def setControl(ctl: Option[ActorRef], flags: Int): Unit = {
    this.ctl      = ctl
    this.ctlFlags = flags
  }

  final def convertToGray(frame: Frame): Mat = {
    val matIn = toMat.convert(frame)
    opencv_imgproc.medianBlur  (matIn, matIn, 5 /* 3 */)
    opencv_imgproc.Laplacian   (matIn, edge , opencv_core.CV_32F, 5 /* 3 */, 1.0, 0.0, opencv_core.BORDER_REPLICATE)
    opencv_imgproc.GaussianBlur(edge , edge , new Size(5, 5), 1.0)
    opencv_imgproc.cvtColor    (edge , gray , opencv_imgproc.COLOR_BGR2GRAY)
    if ((ctlFlags & CtlGray) != 0) ctl.foreach { actor =>
      val matOut = new Mat // (gray.rows(), gray.cols(), opencv_core.CV_8U)
      gray.convertTo(matOut, opencv_core.CV_8U)
      // opencv_imgproc.cvtColor(gray, matOut, opencv_imgproc.COLOR_GRAY2RGB)
      val grayF   = toMat   .convert(matOut)
      val bufImg  = toJava2D.convert(grayF)
      actor ! GrayImage(bufImg)
    }
    gray
  }

  final def convertToBlackAndWhite(in: Mat, thresh: Int): Mat = {
    in.convertTo(blackWhite, opencv_core.CV_8U)
    var y       = 0
    val indexer: UByteRawIndexer = blackWhite.createIndexer()
    val width   = in.cols() // frame.imageWidth
    val height  = in.rows() // frame.imageHeight
    while (y < height) {
      var x = 0
      while (x < width) {
        val v = indexer.get(y, x, 0)
        // if (v < min) min = v
        // if (v > max) max = v
        val b = if (v > thresh) 255 else 0
        indexer.put(y, x, b)
        x += 1
      }
      y += 1
    }

    blackWhite
  }

  private[this] var bwThresh  = 127
  private[this] val minLines  = 640
  private[this] val maxLines  = 2560
  private[this] val lines     = Array.fill(maxLines)(new Line(0, 0, 0, 0))
  private[this] val hough     = new Hough(lines)
  private[this] val analysis  = new Analyze(maxLines)

  protected final val log     = Logging(context.system, this)

  @tailrec
  private def mkHough(in: Mat): Int = {
    val bw        = convertToBlackAndWhite(in, bwThresh)
    val force     = bwThresh > 250
    val numLines  = hough(matIn = bw, force = force)
    val tooHigh   = numLines > maxLines
    if (tooHigh && !force) {
      bwThresh += 2
      log.info(s"inc bw thresh to $bwThresh")
      mkHough(in)
    } else {
      if (numLines < minLines && bwThresh > 10) {
        bwThresh -= 2
        log.info(s"dec bw thresh to $bwThresh")
      }
      if (tooHigh) maxLines else numLines
    }
  }

  final def analyze(frame: Frame): Array[LineI] = {
    val _gray     = convertToGray(frame)
    val bw        = convertToBlackAndWhite(_gray, bwThresh)
    val numLines  = mkHough(_gray)

    if ((ctlFlags & CtlThresh) != 0) ctl.foreach { actor =>
      val frame   = toMat   .convert(blackWhite)
      val bufImg  = toJava2D.convert(frame)
      actor ! ThreshImage(bufImg)
    }

    val width    = bw.cols()
    val height   = bw.rows()
    val numTriLn = analysis.run(lines, numLines0 = numLines, width = width, height = height, config = anaCfg)

    //      var minX, minY, maxX, maxY = 0
    //      res.foreach { ln =>
    //        val x1 = ln.pt1.x
    //        val y1 = ln.pt1.y
    //        val x2 = ln.pt2.x
    //        val y2 = ln.pt2.y
    //        if (x1 < minX) minX = x1
    //        if (x1 > maxX) maxX = x1
    //        if (y1 < minX) minX = y1
    //        if (y1 > maxX) maxX = y1
    //        if (x2 < minY) minY = x2
    //        if (x2 > maxY) maxY = x2
    //        if (y2 < minY) minY = y2
    //        if (y2 > maxY) maxY = y2
    //      }
    //      log.debug(s"analysis yielded ${res.size} lines ($minX, $minY, $maxX, $maxY)")

    val res = new Array[LineI](numTriLn)
    var i = 0
    while (i < numTriLn) {
      res(i) = lines(i).immutable
      i += 1
    }

    res
  }
}