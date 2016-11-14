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
import java.awt.{BasicStroke, Color, EventQueue, Font, Frame, GraphicsDevice, GraphicsEnvironment, Point, RenderingHints}
import java.io.PrintStream
import javax.swing.Timer

import akka.actor.{ActorRef, ActorSystem}

object View {
  final case class Config(verbose: Boolean = false, screenId: String = "", ctlScreenId: String = "",
                          listScreens: Boolean = false,
                          useGrabber: Boolean = false, antiAliasing: Boolean = true,
                          cameraIP: String = "192.168.0.41", cameraPassword: String = "???",
                          useIPCam: Boolean = false, camHAngleStep: Double = 4.0, camVAngle: Double = 0.0)

  def main(args: Array[String]): Unit = {
    val p = new scopt.OptionParser[Config]("Imperfect-RaspiPlayer") {
      //      opt[File]("test-video")
      //        .text ("Test video file")
      //        .required()
      //        .action { (f, c) => c.copy(testVideo = f) }

      //      opt[Int] ('s', "start-frame")
      //        .text ("Start frame index")
      //        .action   { (v, c) => c.copy(startFrame = v) }
      //        .validate {  v     => if (v >= 0) success else failure("start-frame must be >= 0") }

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

      opt[String] ("camera-ip")
        .text ("IP address of the Amcrest IP camera")
        .action   { (v, c) => c.copy(cameraIP = v, useIPCam = true) }

      opt[String] ("camera-password")
        .text ("Password of the Amcrest IP camera (no default - must be provided if not using grabber)")
        .action   { (v, c) => c.copy(cameraPassword = v, useIPCam = true) }

      opt[Double] ("h-angle-step")
        .text ("Camera PTZ horizontal angle step (degrees; default: 4.0)")
        .action   { (v, c) => c.copy(camHAngleStep = v, useIPCam = true) }

      opt[Double] ("v-angle")
        .text ("Camera PTZ vertical angle (degrees; default: 0.0)")
        .action   { (v, c) => c.copy(camVAngle = v, useIPCam = true) }
    }
    p.parse(args, Config()).fold(sys.exit(1)) { config =>
      if (config.listScreens) {
        printScreens(Console.out)
        sys.exit()
      }

      val system      = ActorSystem("system")
      val sourceActor = MainLoop.run(system, config)
      EventQueue.invokeLater(new Runnable {
        def run(): Unit = View.run(system, config, sourceActor)
      })
    }
  }

  private[this] def printScreens(out: PrintStream): Unit = {
    val screens = GraphicsEnvironment.getLocalGraphicsEnvironment.getScreenDevices
    val s = screens.map { dev =>
      val m = dev.getDisplayMode
      s"'${dev.getIDstring}' - ${m.getWidth} x ${m.getHeight}"
    } .sorted.mkString("  ", "\n  ", "")
    out.println(s)
  }

  def run(system: ActorSystem, config: Config, sourceActor: ActorRef): Unit = {
    this.antiAliasing = config.antiAliasing
    import config._
    val screens = GraphicsEnvironment.getLocalGraphicsEnvironment.getScreenDevices
    val opt1: Option[GraphicsDevice] = if (screenId.isEmpty) None else {
      val res = screens.find(_.getIDstring == screenId)
      if (res.isEmpty) {
        warn(s"screen '$screenId' not found.")
        printScreens(Console.err)
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

    lazy val controlWindow = new ControlWindow(system, config, sourceActor)

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
    }

    val t = new Timer(12, new ActionListener {
      def actionPerformed(e: ActionEvent): Unit =
        if (animate) {
          frameIdx = frameIdx + 1
          draw()
        }
    })
    t.setRepeats(true)
    t.start()
  }

  private[this] val fntTest       = new Font(Font.SANS_SERIF, Font.BOLD, 500)
  private[this] var drawRect      = false
  private[this] var drawText      = false
  private[this] var animate       = true
  private[this] var frameIdx      = 0
  private[this] var antiAliasing  = true

  @volatile
  var lines = Array.empty[LineI]

  def paintOffScreen(): Unit = {
    val g = OffScreenG
    g.setColor(Color.black)
    g.fillRect(0, 0, VisibleWidth, VisibleHeight)

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

    {
//      val atOrig = g.getTransform
//      val sx = 1.0
      val sx = 540.0 / 1280 * (16.0/9) / (4.0/3)
      val sy = 540.0 / 1280
      val tx = 100 // (frameIdx * 4) % 1920
      val ty = 0
//      g.scale(1.0, 540.0 / 1280)
//      g.translate((frameIdx * 4) % 1920, 0)
      g.setColor(Color.white)
      g.setStroke(new BasicStroke(2f))
      if (antiAliasing) g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      lines.foreach { ln =>
        val x1 = (ln.x1 * sx + tx).toInt
        val y1 = (ln.y1 * sy + ty).toInt
        val x2 = (ln.x2 * sx + tx).toInt
        val y2 = (ln.y2 * sy + ty).toInt
//        g.drawLine(pt1.x, pt1.y, pt2.x, pt2.y)
        g.drawLine(x1, y1, x2, y2)
      }
//      g.setTransform(atOrig)
    }

    if (drawRect) {
      g.fillRect(0, 0, VisibleWidth, 10)
      g.fillRect(0, VisibleHeight - 10, VisibleWidth, 10)
      g.fillRect(0, 10, 10, VisibleHeight - 20)
      g.fillRect(VisibleWidth - 10, 10, 10, VisibleHeight - 20)

      g.setColor(Color.gray)
      g.fillRect(VisibleWidth/2 - 10, 10, 20, VisibleHeight - 20)
    }
  }

  def quit(): Unit = {
    sys.exit()
  }
}
