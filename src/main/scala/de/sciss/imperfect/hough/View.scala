/*
 *  View.scala
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

import java.awt.event.{ActionEvent, ActionListener, KeyAdapter, KeyEvent, MouseAdapter, MouseEvent}
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, EventQueue, Frame, GraphicsDevice, GraphicsEnvironment, Point, RenderingHints}
import java.io.PrintStream
import javax.imageio.ImageIO
import javax.swing.Timer

import akka.actor.{ActorRef, ActorSystem}
import akka.event.Logging
import de.sciss.file._
import de.sciss.imperfect.hough.MainLoop.Start
import de.sciss.imperfect.hough.Source.Analysis
import de.sciss.imperfect.hough.View.Config
import de.sciss.numbers.IntFunctions
import de.sciss.osc
import de.sciss.osc.UDP

import scala.annotation.switch
import scala.swing.Swing

object View {
  final case class Config(
      verbose       : Boolean       = false,
      screenId      : String        = "",
      ctlScreenId   : String        = "",
      listScreens   : Boolean       = false,
      useGrabber    : Boolean       = false,
      antiAliasing  : Boolean       = true,
      breadCrumbs   : Boolean       = true,
      breadLeft     : Int           = 1,
      breadRight    : Int           = 0,
      cameraIP      : String        = "192.168.0.41",
      cameraPassword: String        = "???",
      useIPCam      : Boolean       = false,
      camHAngleStep : Double        = 1.0,
      camVAngle     : Double        = 0.0,
      templateOut   : Option[File]  = None,
      writeFrames   : Int           = 0,
      maxTriangles  : Int           = 2560 * 2,
      triNumFrames  : Int           = 50,
      strokeWidth   : Double        = 2.0,
      aspect        : Double        = (16.0/9)/(4.0/3),
      acceleration  : Double        = 0.005,
      friction      : Double        = 0.93,
      canvasSpeed   : Int           = 4,
      wipeSpeed     : Int           = 4,
      rattleIP      : String        = "192.168.0.21",
      rattlePort    : Int           = 7771,
      rattlePad     : Int           = 64,
      rattleDump    : Boolean       = false,
      useRattle     : Boolean       = false,
      logging       : Boolean       = false,
      oscPort       : Int           = 57120,
      useOSC        : Boolean       = true
    )

  private val default = Config()

  def main(args: Array[String]): Unit = {
    val p = new scopt.OptionParser[Config]("Imperfect-Hough") {
      opt[String] ('s', "screen")
        .text ("Screen identifier")
        .action   { (v, c) => c.copy(screenId = v) }

      opt[String] ('c', "control-screen")
        .text ("Screen identifier for control window")
        .action   { (v, c) => c.copy(ctlScreenId = v) }

      opt[Unit] ('l', "list-screens")
        .text ("List available screens")
        .action   { (v, c) => c.copy(listScreens = true) }

      opt[Unit] ('v', "verbose")
        .action   { (v, c) => c.copy(verbose = true) }

      opt[Unit] ('g', "grabber")
        .text ("Use video grabber instead of reading files")
        .action   { (v, c) => c.copy(useGrabber = true) }

      opt[Unit] ("no-aa")
        .text ("Disable anti-aliasing")
        .action   { (v, c) => c.copy(antiAliasing = false) }

      opt[Unit] ("no-bread-crumbs")
        .text ("Disable bread-crumb traces")
        .action   { (v, c) => c.copy(breadCrumbs = false) }

      opt[String] ("camera-ip")
        .text (s"IP address of the Amcrest IP camera (default: ${default.cameraIP})")
        .action   { (v, c) => c.copy(cameraIP = v, useIPCam = true) }

      opt[String] ("camera-password")
        .text ("Password of the Amcrest IP camera (no default - must be provided if not using grabber)")
        .action   { (v, c) => c.copy(cameraPassword = v, useIPCam = true) }

      opt[String] ("rattle-ip")
        .text (s"IP address of the rattle computer (default: ${default.rattleIP})")
        .action   { (v, c) => c.copy(rattleIP = v, useRattle = true) }

      opt[Int] ("rattle-port")
        .text (s"OSC port of the rattle computer (default: ${default.rattlePort})")
        .action   { (v, c) => c.copy(rattlePort = v, useRattle = true) }

      opt[Int] ("rattle-pad")
        .text (s"Rattle space padding (default: ${default.rattlePad})")
        .action   { (v, c) => c.copy(rattlePad = v, useRattle = true) }

      opt[Unit] ("rattle-dump")
        .text (s"Dump outgoing rattle messages (default: ${default.rattleDump})")
        .action   { (v, c) => c.copy(rattleDump = true, useRattle = true) }

      opt[Double] ("h-angle-step")
        .text (s"Camera PTZ horizontal angle step (degrees; default: ${default.camHAngleStep})")
        .action   { (v, c) => c.copy(camHAngleStep = v, useIPCam = true) }

      opt[Double] ("v-angle")
        .text (s"Camera PTZ vertical angle (degrees; default: ${default.camVAngle})")
        .action   { (v, c) => c.copy(camVAngle = v, useIPCam = true) }

      opt[File]("output")
        .text ("Write frame sequence as image files (template, use %d as frame-index placeholder).")
        .action { (v, c) => c.copy(templateOut = Some(v)) }

      opt[Int]("write-frames")
        .text ("Maximum number of frames to write as image files (default: 0 - no limit).")
        .action { (v, c) => c.copy(writeFrames = v) }

      opt[Double] ("stroke")
        .text (s"Stroke width (default: ${default.strokeWidth})")
        .action   { (v, c) => c.copy(strokeWidth = v) }

      opt[Double] ("aspect")
        .text (s"Aspect (h/v) of triangle coordinates (default: ${default.aspect})")
        .action   { (v, c) => c.copy(aspect = v) }

      opt[Int] ("triangles")
        .text (s"Maximum number of triangles (default: ${default.maxTriangles})")
        .action   { (v, c) => c.copy(maxTriangles = v) }

      opt[Int] ("step-frames")
        .text (s"Frames between two analyses (default: ${default.triNumFrames})")
        .action   { (v, c) => c.copy(triNumFrames = v) }

      opt[Int] ("bread-left")
        .text (s"Bread-crumbs left wipe (default: ${default.breadLeft})")
        .action   { (v, c) => c.copy(breadLeft = v) }

      opt[Int] ("bread-right")
        .text (s"Bread-crumbs right wipe (default: ${default.breadRight})")
        .action   { (v, c) => c.copy(breadRight = v) }

      opt[Double] ("acceleration")
        .text (s"Acceleration coefficient (default: ${default.acceleration})")
        .action   { (v, c) => c.copy(acceleration = v) }

      opt[Double] ("friction")
        .text (s"Friction coefficient (default: ${default.friction})")
        .action   { (v, c) => c.copy(friction = v) }

      opt[Int] ("canvas-speed")
        .text (s"Canvas speed -- higher is slower (default: ${default.canvasSpeed})")
        .action   { (v, c) => c.copy(canvasSpeed = v) }

      opt[Int] ("wipe-speed")
        .text (s"wipe speed -- higher is slower (default: ${default.wipeSpeed})")
        .action   { (v, c) => c.copy(wipeSpeed = v) }

      opt[Unit] ("log")
        .text ("Enable debug logging")
        .action   { (v, c) => c.copy(logging = true) }

      opt[Unit] ("no-osc")
        .text ("Disable OSC receiver")
        .action   { (v, c) => c.copy(useOSC = false) }

      opt[Int] ("osc-port")
        .text (s"OSC receiver port (default: ${default.oscPort})")
        .action   { (v, c) => c.copy(oscPort = v) }
    }
    p.parse(args, Config()).fold(sys.exit(1)) { config =>
      if (config.listScreens) {
        printScreens(Console.out)
        sys.exit()
      }

      if (config.useOSC) createOSC(config.oscPort)

      val system      = ActorSystem("system")
      if (!config.logging) system.eventStream.setLogLevel(Logging.WarningLevel)
      val source      = Source  (system, config)
      val rattleOpt   = if (config.useRattle) Some(SpaceCommunicator(system, config)) else None
      val loop        = MainLoop(system, source = source, config = config)
      EventQueue.invokeLater(new Runnable {
        def run(): Unit = {
          val view = new View(system, config, source = source, loop = loop, rattleOpt = rattleOpt)
          view.run()
        }
      })
    }
  }

  def createOSC(port: Int): Unit = {
    val config = UDP.Config()
    config.localPort = port
    val r = UDP.Receiver(config)
    r.action = {
      case (osc.Message("/reboot"  ), _) =>
        import sys.process._
        Seq("sudo", "reboot", "now").!
      case (osc.Message("/shutdown"), _) =>
        import sys.process._
        Seq("sudo", "shutdown", "now").!
      case (other, addr) =>
        Console.err.println(s"Received unknown OSC packet $other from $addr")
    }
  }

  def printScreens(out: PrintStream): Unit = {
    val screens = GraphicsEnvironment.getLocalGraphicsEnvironment.getScreenDevices
    val s = screens.map { dev =>
      val m = dev.getDisplayMode
      s"'${dev.getIDstring}' - ${m.getWidth} x ${m.getHeight}"
    } .sorted.mkString("  ", "\n  ", "")
    out.println(s)
  }
}
final class View(system: ActorSystem, config: Config, source: ActorRef, loop: ActorRef, rattleOpt: Option[ActorRef]) {
  private[this] val writeOutput   = config.templateOut.isDefined
  private[this] val strkLines     = new BasicStroke(config.strokeWidth.toFloat)

  import config.{antiAliasing, breadLeft, breadRight, triNumFrames}

  def run(): Unit = {
    import config._
    val screens = GraphicsEnvironment.getLocalGraphicsEnvironment.getScreenDevices
    val opt1: Option[GraphicsDevice] = if (screenId.isEmpty) None else {
      val res = screens.find(_.getIDstring == screenId)
      if (res.isEmpty) {
        warn(s"screen '$screenId' not found.")
        View.printScreens(Console.err)
      }
      res
    }
    val screen = opt1.getOrElse {
      val opt2 = screens.find { dev =>
        val m = dev.getDisplayMode
        m.getWidth == NominalWidth && m.getHeight == NominalHeight
      }
      if (opt2.isEmpty) {
        warn(s"no screen of size $NominalWidth x $NominalHeight found")
      }
      opt2.getOrElse(screens.head)
    }

    lazy val controlWindow = new ControlWindow(system, config, source)

    var haveWarnedWinSize = false

    val screenConf = screen.getDefaultConfiguration
    val w = new Frame(null, screenConf) {
      setUndecorated  (true)
      setIgnoreRepaint(true)
    }
    w.addKeyListener(new KeyAdapter {
      override def keyTyped  (e: KeyEvent): Unit = ()
      override def keyPressed(e: KeyEvent): Unit = {
        e.getKeyCode match {
          case KeyEvent.VK_ESCAPE => quit()
          case KeyEvent.VK_R      => drawRect = !drawRect // ; w.repaint()
          case KeyEvent.VK_T      => drawText = !drawText
          case KeyEvent.VK_A      => animate  = !animate
          case KeyEvent.VK_C      => controlWindow.open()

          case _ =>
        }
      }
    })
    w.addMouseListener(new MouseAdapter {
      override def mousePressed(e: MouseEvent): Unit = w.requestFocus()
    })
    // WARNING: setSize breaks buffer-strategy based painting ?
    //    w.setSize(NominalWidth, NominalHeight)
    w.setSize(screenConf.getBounds.getSize)
    screen.setFullScreenWindow(w)
    w.requestFocus()

    // "hide" cursor
    val cursorImg = new BufferedImage(16, 16, BufferedImage.TYPE_INT_ARGB)
    val cursor = w.getToolkit.createCustomCursor(cursorImg, new Point(0, 0), "blank")
    w.setCursor(cursor)

    // Ok, so there is some weird bug in that sometime the
    // buffer doesn't have the correct size. For now, it
    // seems, waiting with the thread a bit helps.
    Thread.sleep(50)
    w.createBufferStrategy(2)
    Thread.sleep(50)

    val strategy = w.getBufferStrategy

    def draw(): Unit = {
      paintOffScreen()
      val width  = w.getWidth
      val height = w.getHeight
      do {
        do {
          val g = strategy.getDrawGraphics
          if (width == NominalWidth && height == NominalHeight) {
            g.drawImage(OffScreenImg,            0,             0, NominalWidth, VisibleHeight,
                                                 0,             0, NominalWidth, VisibleHeight, null)
            g.drawImage(OffScreenImg,            0, VisibleHeight, NominalWidth, NominalHeight,
                                      NominalWidth,             0, VisibleWidth, VisibleHeight, null)
          } else {
            if (!haveWarnedWinSize) {
              warn(s"Full screen window has dimensions $width x $height instead of $NominalWidth x $NominalHeight")
              haveWarnedWinSize = true
            }
            g.drawImage(OffScreenImg,            0,        0, width,        height/2,
                                                 0,        0, NominalWidth, VisibleHeight, null)
            g.drawImage(OffScreenImg,            0, height/2, width,        height,
                                      NominalWidth,        0, VisibleWidth, VisibleHeight, null)
            g.dispose()
          }
        } while (strategy.contentsRestored())
        strategy.show()
      } while (strategy.contentsLost())

      if (writeOutput && (config.writeFrames <= 0 || frameIdx <= config.writeFrames)) {
        val temp = config.templateOut.get
        val fOut = temp.parent / temp.name.format(frameIdx)
        if (!fOut.exists()) {
          val img = new BufferedImage(NominalWidth, NominalHeight,
            if (config.antiAliasing) BufferedImage.TYPE_BYTE_GRAY else BufferedImage.TYPE_BYTE_BINARY)
          val g = img.createGraphics()
          g.drawImage(OffScreenImg,            0,             0, NominalWidth, VisibleHeight,
                                               0,             0, NominalWidth, VisibleHeight, null)
          g.drawImage(OffScreenImg,            0, VisibleHeight, NominalWidth, NominalHeight,
                                    NominalWidth,             0, VisibleWidth, VisibleHeight, null)
          g.dispose()
          val fmt = fOut.ext match {
            case "png"  => "png"
            case _      => "jpg"
          }
          ImageIO.write(img, fmt, fOut)
          img.flush()
        }
      }
    }

    // ---- animation timer ----
    val t = new Timer(12, new ActionListener {
      def actionPerformed(e: ActionEvent): Unit =
        if (animate) {
          frameIdx = frameIdx + 1
          draw()
        }
    })
    t.setRepeats(true)
    t.start()

    // ---- algorithm loop ----
    loop ! Start(this)
  }

//  private[this] val fntTest       = new Font(Font.SANS_SERIF, Font.BOLD, 500)
  private[this] var drawRect      = false
  private[this] var drawText      = false
  private[this] var animate       = true
  private[this] var frameIdx      = 0

  def update(a: Analysis): Unit = {
    Swing.onEDT {
      if (triPhase >= triNumFrames) {
        if (anaPending == null) {
          setCurrentAnalysis(a)
          loop ! Source.Task
        } else {
          setCurrentAnalysis(anaPending)
          anaPending = a
        }
      } else {
        if (anaPending != null) warn("Pending analysis overflow!")
        anaPending = a
      }
    }
  }

  private def setCurrentAnalysis(a: Analysis): Unit = {
    anaCurrent      = a
    triPhase        = 0
    analysisFrames += 1
    val isEven      = analysisFrames % 2 == 0

    val _triPI  = a.triPrev
    val _triN   = a.triNext
    val _triP   = trianglePrev
    val _veloP  = if (isEven) velocities1 else velocities2
    val _veloN  = if (isEven) velocities2 else velocities1
    var i = 0
    var j = 0
    while (i < _triPI.length) {
      val triI = _triPI(i)
      if (triI.isIncoherent) {
        val tri = _triP (j)
        val vp  = _veloP(i)
        tri.x1  = vp.x1.toInt
        tri.y1  = vp.y1.toInt
        tri.x2  = vp.x2.toInt
        tri.y2  = vp.y2.toInt
        tri.x3  = vp.x3.toInt
        tri.y3  = vp.y3.toInt
        j += 1
      }
      i += 1
    }
    numTriPrev = j

    val _perm = Coherence.permutations
    i = 0
    while (i < _triN.length) {
      val tri = _triN(i)
      val vn  = _veloN(i)
      if (tri.isCoherent) {
        val perm = _perm(tri.prevPerm)
        vn.copyFromPerm(_veloP(tri.prevIndex), perm)
      } else {
        vn.copyFrom(tri)
      }
      i += 1
    }
  }

  private[this] var anaPending: Analysis = _
  private[this] var anaCurrent: Analysis = Analysis(Array.empty, Array.empty)
  private[this] val trianglePrev = Array.fill(config.maxTriangles)(new Triangle(0, 0, 0, 0, 0, 0, 0))
  private[this] var numTriPrev   = 0
  private[this] val velocities1  = Array.fill(config.maxTriangles)(new Velocities(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
  private[this] val velocities2  = Array.fill(config.maxTriangles)(new Velocities(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
  private[this] var triPhase  = triNumFrames
  private[this] val breadLeftRight  = config.breadLeft + config.breadRight

  private[this] var analysisFrames = 0

  private[this] val line      = new Line(0, 0, 0, 0)

  private[this] val atanMul1  = 4.0
  private[this] val atanAdd1  = -atanMul1/2
  private[this] val atanAdd2  = math.atan(atanMul1/2)
  private[this] val atanMul2  = 1.0 / (2 * atanAdd2)

  private[this] val sx = 0.5 * config.aspect
  private[this] val sy = 0.5

  private[this] val cvRecip = 1.0f / config.canvasSpeed
  private[this] val wvRecip = 1.0f / config.wipeSpeed

  private[this] val rattle = rattleOpt.orNull

  // 0 - open black (rattle) rectangle, do not pan
  // 1 - expand black rectangle and pan at the same time
  // 2 - halt black rectangle and keep panning
  // 3 - shrink black rectangle / erase traces
  // then repeat
  private[this] var algoStage: Int = _

  private[this] val bw0           = ((NominalWidth + breadLeftRight) * sx).toInt
  private[this] val fullTriWidth  = NominalWidth * sx

  private[this] var rattleRectX0: Float = _
  private[this] var rattleRectX1: Float = _
  private[this] var triRectX0   : Float = _
  private[this] var triRectX1   : Float = _

  def resetStage(): Unit = {
    algoStage     = 0
    rattleRectX0  = 0f
    rattleRectX1  = 0f
    triRectX0     = 0f // + config.rattlePad
    triRectX1     = 0f - config.rattlePad
  }

  resetStage()

  private[this] var STAT_DP_SPACE = 0L
  private[this] var STAT_HH_SPACE = 0L

  def paintOffScreen(): Unit = {
    val g   = OffScreenG
    val _sx = sx
    val _sy = sy
    //      val tx = (analysisFrames * 6) % 1920
    //      val tx = (frameIdx % (1920 * 4)) * 0.25
    //      val tx = frameIdx * 0.25
    // val tx  = (frameIdx % (VisibleWidth * config.canvasSpeed)) * cvRecip
    val tx0 = triRectX0
    val tx1 = triRectX1
    val tx2 = tx0 - config.rattlePad
    val bx1 = tx0.toInt - breadLeft
    val bx2 = tx1.toInt + breadRight

    val rx0 = rattleRectX0.toInt
    val rx1 = rattleRectX1.toInt

    if (rattle != null) {
      rattle ! SpaceCommunicator.Position(rx0, rx1)
    }

//    {
//      g.setColor(Color.red)
//      val foo0 = IntFunctions.wrap(rattleRectX0.toInt, 0, VisibleWidth)
//      val foo1 = IntFunctions.wrap(rattleRectX1.toInt, 0, VisibleWidth)
//      if (foo0 <= foo1) {
//        g.fillRect(foo0, 0, foo1 - foo0, VisibleHeight)
//      } else {
//        g.fillRect(foo0, 0, VisibleWidth - foo0, VisibleHeight)
//        g.fillRect(0, 0, foo1, VisibleHeight)
//      }
//    }

    g.setColor(Color.black)
    if (config.breadCrumbs) {
      val w1 = math.min(bw0, VisibleWidth - bx1)
      g.fillRect(bx1, 0, w1, VisibleHeight)
      // val w2 = bw0 - w1
      // if (w2 > 0) g.fillRect(0, 0, w2, VisibleHeight)

    } else {
      g.fillRect(0, 0, VisibleWidth, VisibleHeight)
    }

    /*
    if (drawText) {
      val atOrig = g.getTransform
      val rot = frameIdx * Math.PI / 180
      g.rotate(rot, VisibleWidth/2, VisibleHeight/2)
      g.setColor(Color.white)
      g.setFont(fntTest)
      val fm = g.getFontMetrics
      g.drawString("Imperfect Reconstruction", 20, fm.getAscent + 20)
      g.setTransform(atOrig)
    }
    */

    val ty = 0
    g.setColor(Color.white)
    g.setStroke(strkLines)
    if (antiAliasing) g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    val inTime    = triPhase <= triNumFrames
    val phase     = triPhase.toDouble / triNumFrames
    val fadeIn    = (math.atan(math.min(1.0, phase) * atanMul1 + atanAdd1) + atanAdd2) * atanMul2
    val fadeOut   = 1.0 - fadeIn

    val _ln     = line
    val _triP   = trianglePrev
    val _triPSz = numTriPrev
    val _triN   = anaCurrent.triNext
    val isEven  = analysisFrames % 2 == 0
    val _velo   = if (isEven) velocities2 else velocities1

    // automatically wraps around visible width.
    // lines that "break" across the boundary are simply not drawn.
    def drawLine(x1: Int, y1: Int, x2: Int, y2: Int): Unit = {
      if (x1 >= tx2 && x2 >= tx2 && x1 <= bx2 && x2 <= bx2) {
        val b   = x1 <= x2
        val x1m = IntFunctions.wrap(x1, 0, VisibleWidth)
        val x2m = IntFunctions.wrap(x2, 0, VisibleWidth)
        val bm  = x1m <= x2m
        if (b == bm) {
          g.drawLine(x1m, y1, x2m, y2)
        }
      }
    }

    val accel = config.acceleration.toFloat
    val fric  = config.friction    .toFloat

    var i = 0
    if (inTime) while (i < _triPSz) {
      val tri = _triP(i)

      _ln.x1 = tri.x1
      _ln.y1 = tri.y1
      _ln.x2 = tri.x2
      _ln.y2 = tri.y2
      Analyze.resizeLine(_ln, _ln, width = NominalWidth, height = NominalHeight, factor = fadeOut)
      val x11 = (_ln.x1 * _sx + tx0).toInt
      val y11 = (_ln.y1 * _sy + ty).toInt
      val x21 = (_ln.x2 * _sx + tx0).toInt
      val y21 = (_ln.y2 * _sy + ty).toInt
      drawLine(x11, y11, x21, y21)

      _ln.x1 = tri.x2
      _ln.y1 = tri.y2
      _ln.x2 = tri.x3
      _ln.y2 = tri.y3
      Analyze.resizeLine(_ln, _ln, width = NominalWidth, height = NominalHeight, factor = fadeOut)
      val x12 = (_ln.x1 * _sx + tx0).toInt
      val y12 = (_ln.y1 * _sy + ty).toInt
      val x22 = (_ln.x2 * _sx + tx0).toInt
      val y22 = (_ln.y2 * _sy + ty).toInt
      drawLine(x12, y12, x22, y22)

      _ln.x1 = tri.x3
      _ln.y1 = tri.y3
      _ln.x2 = tri.x1
      _ln.y2 = tri.y1
      Analyze.resizeLine(_ln, _ln, width = NominalWidth, height = NominalHeight, factor = fadeOut)
      val x13 = (_ln.x1 * _sx + tx0).toInt
      val y13 = (_ln.y1 * _sy + ty).toInt
      val x23 = (_ln.x2 * _sx + tx0).toInt
      val y23 = (_ln.y2 * _sy + ty).toInt
      drawLine(x13, y13, x23, y23)

      i += 1
    }

    i = 0
    while (i < _triN.length) {
      val tri = _triN(i)
      val v   = _velo(i)
      if (tri.isIncoherent) {
        _ln.x1 = v.x1.toInt
        _ln.y1 = v.y1.toInt
        _ln.x2 = v.x2.toInt
        _ln.y2 = v.y2.toInt
        Analyze.resizeLine(_ln, _ln, width = NominalWidth, height = NominalHeight, factor = fadeIn)
        val x11 = (_ln.x1 * _sx + tx0).toInt
        val y11 = (_ln.y1 * _sy + ty).toInt
        val x21 = (_ln.x2 * _sx + tx0).toInt
        val y21 = (_ln.y2 * _sy + ty).toInt
        drawLine(x11, y11, x21, y21)

        _ln.x1 = v.x2.toInt
        _ln.y1 = v.y2.toInt
        _ln.x2 = v.x3.toInt
        _ln.y2 = v.y3.toInt
        Analyze.resizeLine(_ln, _ln, width = NominalWidth, height = NominalHeight, factor = fadeIn)
        val x12 = (_ln.x1 * _sx + tx0).toInt
        val y12 = (_ln.y1 * _sy + ty).toInt
        val x22 = (_ln.x2 * _sx + tx0).toInt
        val y22 = (_ln.y2 * _sy + ty).toInt
        drawLine(x12, y12, x22, y22)

        _ln.x1 = v.x3.toInt
        _ln.y1 = v.y3.toInt
        _ln.x2 = v.x1.toInt
        _ln.y2 = v.y1.toInt
        Analyze.resizeLine(_ln, _ln, width = NominalWidth, height = NominalHeight, factor = fadeIn)
        val x13 = (_ln.x1 * _sx + tx0).toInt
        val y13 = (_ln.y1 * _sy + ty).toInt
        val x23 = (_ln.x2 * _sx + tx0).toInt
        val y23 = (_ln.y2 * _sy + ty).toInt
        drawLine(x13, y13, x23, y23)

      } else {
//          val f    = 0.01f
//          val r    = 0.98f
        val ax1  = (tri.x1 - v.x1) * accel
        v.vx1    = (v.vx1 + ax1) * fric
        v.x1    += v.vx1
        val ay1  = (tri.y1 - v.y1) * accel
        v.vy1    = (v.vy1 + ay1) * fric
        v.y1    += v.vy1
        val ax2  = (tri.x2 - v.x2) * accel
        v.vx2    = (v.vx2 + ax2) * fric
        v.x2    += v.vx2
        val ay2  = (tri.y2 - v.y2) * accel
        v.vy2    = (v.vy2 + ay2) * fric
        v.y2    += v.vy2
        val ax3  = (tri.x3 - v.x3) * accel
        v.vx3    = (v.vx3 + ax3) * fric
        v.x3    += v.vx3
        val ay3  = (tri.y3 - v.y3) * accel
        v.vy3    = (v.vy3 + ay3) * fric
        v.y3    += v.vy3

        val x1   = (v.x1 * _sx + tx0).toInt
        val y1   = (v.y1 * _sy + ty).toInt
        val x2   = (v.x2 * _sx + tx0).toInt
        val y2   = (v.y2 * _sy + ty).toInt
        val x3   = (v.x3 * _sx + tx0).toInt
        val y3   = (v.y3 * _sy + ty).toInt
        drawLine(x1, y1, x2, y2)
        drawLine(x2, y2, x3, y3)
        drawLine(x3, y3, x1, y1)
      }
      i += 1
    }

    triPhase += 1
    if (triPhase >= triNumFrames && anaPending != null) {
      setCurrentAnalysis(anaPending)
      anaPending  = null
      loop ! Source.Task
    }

    {
      val foo0 = IntFunctions.wrap(rx0, 0, VisibleWidth)
      val foo1 = IntFunctions.wrap(rx1, 0, VisibleWidth)
      if (foo0 <= foo1) {
        STAT_HH_SPACE += foo1 - foo0
        STAT_DP_SPACE += foo0
        STAT_DP_SPACE += VisibleWidth - foo1
      } else {
        STAT_HH_SPACE += VisibleWidth - foo0
        STAT_HH_SPACE += foo1
        STAT_DP_SPACE += foo0
        STAT_DP_SPACE += foo0 - foo1
      }
    }
    STAT_DP_SPACE += (rx1 - rx0)

    // 0 - open black (rattle) rectangle, do not pan
    // 1 - expand black rectangle and pan at the same time
    // 2 - halt black rectangle and keep panning
    // 3 - shrink black rectangle / erase traces
    // then repeat
    (algoStage: @switch) match {
      case 0 =>
        rattleRectX1 += wvRecip
        triRectX1    += wvRecip
        if (triRectX1 - triRectX0 >= fullTriWidth) algoStage += 1

      case 1 =>
        rattleRectX1 = math.min(rattleRectX0 + VisibleWidth, rattleRectX1 + cvRecip)
        triRectX0    += cvRecip
        triRectX1    += cvRecip
//        if (rattleRectX1 - rattleRectX0 >= VisibleWidth) algoStage += 1
        if (triRectX1 >= VisibleWidth) algoStage += 1

      case 2 =>
//        triRectX0    += wvRecip
        triRectX0    += cvRecip
        if (triRectX0 >= triRectX1) algoStage += 1

      case 3 =>
        triRectX0    += wvRecip
        val x1  = rattleRectX0.toInt
        rattleRectX0 += wvRecip
        val x2  = rattleRectX0.toInt + 1
        val x1m = IntFunctions.wrap(x1, 0, VisibleWidth)
        val x2m = IntFunctions.wrap(x2, 0, VisibleWidth)

        g.setColor(Color.black)
        if (x1m <= x2m) {
          g.fillRect(x1m, 0, x2m - x1m, VisibleHeight)
        } else {
          g.fillRect(x1m, 0, VisibleWidth - x1m, VisibleHeight)
          g.fillRect(0, 0, x2m, VisibleHeight)
        }

        if (rattleRectX0 >= rattleRectX1) {
          val STAT_TOT = (STAT_DP_SPACE + STAT_HH_SPACE).toDouble
          println(f"DP space = $STAT_DP_SPACE or ${STAT_DP_SPACE / STAT_TOT * 100}%1.1f")
          println(f"HH space = $STAT_HH_SPACE or ${STAT_HH_SPACE / STAT_TOT * 100}%1.1f")
          resetStage()
        }
    }


    if (drawRect) {
      //      g.fillRect(0, 0, VisibleWidth, 10)
      //      g.fillRect(0, VisibleHeight - 10, VisibleWidth, 10)
      //      g.fillRect(0, 10, 10, VisibleHeight - 20)
      //      g.fillRect(VisibleWidth - 10, 10, 10, VisibleHeight - 20)
      //
      //      g.setColor(Color.gray)
      //      g.fillRect(VisibleWidth/2 - 10, 10, 20, VisibleHeight - 20)
      val foo0 = IntFunctions.wrap(rx0, 0, VisibleWidth)
      val foo1 = IntFunctions.wrap(rx1, 0, VisibleWidth)
      if (foo0 <= foo1) {
        g.setColor(Color.red)
        g.fillRect(foo0, 0, foo1 - foo0, 32)
        g.setColor(Color.green)
        g.fillRect(0, 0, foo0, 32)
        g.fillRect(foo1, 0, VisibleWidth - foo1, 32)
      } else {
        g.setColor(Color.red)
        g.fillRect(foo0, 0, VisibleWidth - foo0, VisibleHeight)
        g.fillRect(0, 0, foo1, VisibleHeight)
        g.setColor(Color.green)
        g.fillRect(0, 0, foo0, 32)
        g.fillRect(foo1, 0, foo0 - foo1, 32)
      }
    }

    // println(f"algo $algoStage tx0 $triRectX0%g tx1 $triRectX1%g rx0 $rattleRectX0%g, rx1 $rattleRectX1%g")
  }

  def quit(): Unit = {
    sys.exit()
  }
}