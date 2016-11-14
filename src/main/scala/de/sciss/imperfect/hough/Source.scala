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
  final case class Analysis(triPrev: Array[TriangleI], triNext: Array[TriangleI])

  final case class GrayImage  (img: BufferedImage)
  final case class ThreshImage(img: BufferedImage)
  final case class LinesIn    (arr: Array[LineI])
  final case class LinesExt   (arr: Array[LineI])

  def live (): Props = Props(new SourceLive)
  def files(): Props = Props(new SourceFiles)
  def ipCam(ip: String, password: String, hAngleStep: Double, vAngle: Double): Props =
    Props(new SourceIPCam(ip = ip, password = password, hAngleStep = hAngleStep, vAngle = vAngle))


  final val CtlNone     = 0x00
  final val CtlGray     = 0x01
  final val CtlThresh   = 0x02
  final val CtlLinesIn  = 0x04
  final val CtlLinesExt = 0x08
}
abstract class SourceLike extends Actor {
  import Source._

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

  private[this] var bwThresh      = 127
  private[this] val minLines      = 640
  private[this] val maxLines      = 2560
  private[this] val maxTriangles  = maxLines/2
  private[this] val lines1        = Array.fill (maxLines)(new Line(0, 0, 0, 0))
  //  private[this] val lines2    = Array.fill (maxLines)(new Line(0, 0, 0, 0))
  private[this] val hough         = new Hough  (maxLines)
  private[this] val analysis      = new Analyze(maxLines)

  private[this] val triangles1    = Array.fill    (maxTriangles)(new Triangle(0, 0, 0, 0, 0, 0))
  private[this] val triangles2    = Array.fill    (maxTriangles)(new Triangle(0, 0, 0, 0, 0, 0))
//  private[this] val triIndices1   = new Array[Int](maxTriangles)
//  private[this] val triIndices2   = new Array[Int](maxTriangles)
  private[this] val coherence     = new Coherence (maxTriangles)
  private[this] var numTriPrev    = 0

  protected final val log         = Logging(context.system, this)

  @tailrec
  private def mkHough(in: Mat, lines: Array[Line]): Int = {
    val bw        = convertToBlackAndWhite(in, bwThresh)
    val force     = bwThresh > 250
    val numLines  = hough.run(matIn = bw, lines = lines, force = force)
    val tooHigh   = numLines > maxLines
    if (tooHigh && !force) {
      bwThresh += 2
      log.info(s"inc bw thresh to $bwThresh")
      mkHough(in, lines)
    } else {
      if (numLines < minLines && bwThresh > 10) {
        bwThresh -= 2
        log.info(s"dec bw thresh to $bwThresh")
      }
      if (tooHigh) maxLines else numLines
    }
  }

  private[this] var flipFlop = false

  final def analyze(frame: Frame, dir: Int): Analysis = {
    val _gray       = convertToGray(frame)
    val bw          = convertToBlackAndWhite(_gray, bwThresh)
    val lines       = lines1
    val triPrev     = if (flipFlop) triangles1  else triangles2
    val triNext     = if (flipFlop) triangles2  else triangles1
//    val triIdxPrev  = if (flipFlop) triIndices1 else triIndices2
//    val triIdxNext  = if (flipFlop) triIndices2 else triIndices1

    flipFlop      = !flipFlop
    var numLines  = mkHough(_gray, lines)

    if ((ctlFlags & CtlThresh) != 0) ctl.foreach { actor =>
      val frame   = toMat   .convert(blackWhite)
      val bufImg  = toJava2D.convert(frame)
      actor ! ThreshImage(bufImg)
    }

    val width    = bw.cols()
    val height   = bw.rows()
//    val numTriLn = analysis.run(lines, numLines0 = numLines, width = width, height = height, config = anaCfg)

    if (anaCfg.filterSim) numLines = Analyze.removeSimilarLines(lines, numLines)

    if ((ctlFlags & CtlLinesIn) != 0) ctl.foreach { actor =>
      val arr = new Array[LineI](numLines)
      var i = 0
      while (i < arr.length) {
        arr(i) = lines(i).immutable
        i += 1
      }
      actor ! LinesIn(arr)
    }

    if (anaCfg.useExtend) {
      analysis.extendLines(lines, numLines = numLines, width = width, height = height)
    }

    if ((ctlFlags & CtlLinesExt) != 0) ctl.foreach { actor =>
      val arr = new Array[LineI](numLines)
      var i = 0
      while (i < arr.length) {
        arr(i) = lines(i).immutable
        i += 1
      }
      actor ! LinesExt(arr)
    }

    analysis.calcIntersections(lines, numLines = numLines, minAngDeg = anaCfg.minAngDeg)
    val numTriNext = analysis.findTriangles(lines = lines, numLines = numLines, triangles = triNext,
      minTriLen = anaCfg.minTriLen, width = width, height = height)

    val baseTol   = 80 // 40
    val leftTol   = math.max(0, -dir) * baseTol + baseTol
    val rightTol  = math.max(0,  dir) * baseTol + baseTol
    val numMatch = coherence.run(triPrev = triPrev, numTriPrev = numTriPrev, triNext = triNext,
      numTriNext = numTriNext, // indicesPrev = triIdxPrev, indicesNext = triIdxNext,
      leftTol = leftTol, rightTol = rightTol, topTol = baseTol, bottomTol = baseTol)

    log.info(s"found $numMatch triangle matches ${if (numTriNext == 0) "" else s"(${numMatch * 100 / numTriNext}%)"}")

    val _numTriPrev = numTriPrev
    val resPrev = new Array[TriangleI](_numTriPrev)
    var i = 0
    while (i < _numTriPrev) {
      resPrev(i) = triPrev(i).immutable
      i += 1
    }

    val resNext = new Array[TriangleI](numTriNext)
    i = 0
    while (i < numTriNext) {
      resNext(i) = triNext(i).immutable
      i += 1
    }

    numTriPrev = numTriNext

    Analysis(triPrev = resPrev, triNext = resNext)
  }
}