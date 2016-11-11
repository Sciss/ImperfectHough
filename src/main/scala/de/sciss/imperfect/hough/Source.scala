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

import akka.actor.{Actor, Props}
import akka.event.Logging
import de.sciss.file._
import de.sciss.imperfect.hough.Analyze.Line
import de.sciss.kollflitz.Vec
import org.bytedeco.javacpp.indexer.FloatRawIndexer
import org.bytedeco.javacpp.opencv_core.{Mat, Size}
import org.bytedeco.javacpp.{opencv_core, opencv_imgcodecs, opencv_imgproc}
import org.bytedeco.javacv.FrameGrabber.ImageMode
import org.bytedeco.javacv.{Frame, OpenCVFrameConverter, OpenCVFrameGrabber}

object Source {
  final case class Open(width: Int, height: Int)
  case object Task
  case object Close

  def live (): Props = Props(new SourceLive)
  def files(): Props = Props(new SourceFiles)
  def ipCam(ip: String, password: String, hAngleStep: Double, vAngle: Double): Props =
    Props(new SourceIPCam(ip = ip, password = password, hAngleStep = hAngleStep, vAngle = vAngle))
}
abstract class SourceLike {
  _: Actor =>

  protected final val toMat   = new OpenCVFrameConverter.ToMat
  private[this]   val anaCfg  = Analyze.Config()

  def analyze(frame: Frame): Vec[Line] = {
    val matIn = toMat.convert(frame)
    val edge  = new Mat
    val blur  = new Mat
    val gray  = new Mat
    //      val canny = new Mat
    // void cv::Sobel(InputArray src, OutputArray dst, int ddepth, int dx, int dy,
    //                int ksize = 3, double scale = 1, double delta = 0, int borderType = BORDER_DEFAULT
    //      opencv_imgproc.Sobel(/* src = */ matIn, /* dst = */ sobel, /* ddepth = */ opencv_core.CV_32F,
    //        /* dx = */ 1, /* dy = */ 1, /* ksize = */ 3, /* scale = */ 1.0,
    //        /* delta = */ 0.0, /* borderType = */ opencv_core.BORDER_DEFAULT)
    //      opencv_imgproc.GaussianBlur(???, ???, ???, ???)
    //      opencv_imgproc.cvtColor(matIn, gray, opencv_imgproc.COLOR_BGR2GRAY)
    //      opencv_imgproc.Canny(gray, canny, 40.0 /* 80.0 */ /* 40.0 */, 200.0, 3, false)
    //      // XXX TODO --- hough only accepts gray -- binary output of canny fails

    opencv_imgproc.medianBlur(matIn, matIn, 3)
    opencv_imgproc.Laplacian(matIn, edge, opencv_core.CV_32F, 5 /* 3 */, 1.0, 0.0, opencv_core.BORDER_REPLICATE)
    opencv_imgproc.GaussianBlur(edge, blur, new Size(5, 5), 1.0)
    //  opencv_core.normalize(blur, blur)
    opencv_imgproc.cvtColor(blur, gray, opencv_imgproc.COLOR_BGR2GRAY)
    //  opencv_imgproc.Canny(canny, canny, 80.0 /* 40.0 */, 200.0, 3, false)
    //  opencv_imgproc.equalizeHist(gray, gray)

    //  val minPtr = new DoublePointer(1)
    //  val maxPtr = new DoublePointer(1)
    //  opencv_core.minMaxLoc(gray, minPtr, maxPtr, null, null, null)
    //  println(s"min = ${minPtr.get()}; max = ${maxPtr.get()}")

    val indexer = gray.createIndexer[FloatRawIndexer]()
    //  println(s"sizes = ${indexer.sizes().mkString("[", ", ", "]")}")
    // var min     = Float.MaxValue
    // var max     = Float.MinValue
    val thresh  = 127f
    var y       = 0
    val width   = frame.imageWidth
    val height  = frame.imageHeight
    while (y < height) {
      var x = 0
      while (x < width) {
        val v = indexer.get(y, x, 0)
        // if (v < min) min = v
        // if (v > max) max = v
        val b = if (v > thresh) 255f else 0f
        indexer.put(y, x, b)
        x += 1
      }
      y += 1
    }

    //      println(s"TYPE = ${gray.`type`()}")
    val gray8 = new Mat
    gray.convertTo(gray8, opencv_core.CV_8U)
    val res = Analyze.run(gray8 /* canny */, anaCfg)

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

    res
  }
}
final class SourceLive extends SourceLike with Actor {
  import Source._

  private[this] val log     = Logging(context.system, this)
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
final class SourceFiles extends SourceLike with Actor {
  import Source._

  private[this] val log     = Logging(context.system, this)
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
      val res = analyze(frame)
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