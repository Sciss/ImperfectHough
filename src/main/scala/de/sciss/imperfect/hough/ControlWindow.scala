/*
 *  ControlWindow.scala
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
import java.awt.{Color, GraphicsConfiguration, GraphicsEnvironment, RenderingHints}

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.event.Logging
import de.sciss.imperfect.hough.View.Config

import scala.swing.event.ButtonClicked
import scala.swing.{BorderPanel, CheckBox, Component, Dimension, FlowPanel, Frame, Graphics2D, Swing}

class ControlWindow(system: ActorSystem, config: Config, source: ActorRef) {
  private[this] val gc: Option[GraphicsConfiguration] = if (config.ctlScreenId.isEmpty) None else {
    val screens = GraphicsEnvironment.getLocalGraphicsEnvironment.getScreenDevices
    val res = screens.find(_.getIDstring == config.ctlScreenId)
    if (res.isEmpty) {
      warn(s"control-screen '${config.ctlScreenId}' not found.")
    }
    res.map(_.getDefaultConfiguration)
  }

  private[this] var flags = 0

  private[this] var image   : BufferedImage = _
  private[this] var linesIn : Array[LineI]  = _
  private[this] var linesExt: Array[LineI]  = _

  private[this] val ggGrayImage = new CheckBox("Gray Image") {
    listenTo(this)
    reactions += {
      case ButtonClicked(_) => updateFlags()
    }
  }

  private[this] val ggThreshImage = new CheckBox("Thresh Image") {
    listenTo(this)
    reactions += {
      case ButtonClicked(_) => updateFlags()
    }
  }

  private[this] val ggLinesIn = new CheckBox("Input Lines") {
    listenTo(this)
    reactions += {
      case ButtonClicked(_) =>
        updateFlags()
        if (!selected) linesIn = null
    }
  }

  private[this] val ggLinesExt = new CheckBox("Extended Lines") {
    listenTo(this)
    reactions += {
      case ButtonClicked(_) =>
        updateFlags()
        if (!selected) linesExt = null
    }
  }

  private[this] val ggImageView = new Component {
    private[this] val colrGreen = new Color(0, 0x80, 0)

    opaque        = true
    preferredSize = new Dimension(1920/2, 1080/2)

    override protected def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      val w = peer.getWidth
      val h = peer.getHeight
      g.scale(w / 1920.0, h / 1080.0)
      if (image == null) {
        g.setColor(colrGreen)
        g.fillRect(0, 0, 1920, 1080)
      } else {
        g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
        g.drawImage(image, 0, 0, null)
      }

      if (linesExt != null) {
        g.setColor(Color.red)
        linesExt.foreach { ln =>
          g.drawLine(ln.x1, ln.y1, ln.x2, ln.y2)
        }
      }
      if (linesIn != null) {
        g.setColor(Color.green)
        linesIn.foreach { ln =>
          g.drawLine(ln.x1, ln.y1, ln.x2, ln.y2)
        }
      }
    }
  }

  private[this] val frame: Frame = new Frame(gc.orNull) {
    title = "Hough Control"
    contents = new BorderPanel {
      add(new FlowPanel(ggGrayImage, ggThreshImage, ggLinesIn, ggLinesExt), BorderPanel.Position.North )
      add(ggImageView, BorderPanel.Position.Center)
    }
    pack().centerOnScreen()
  }

  private final class ActorImpl extends Actor {
    private[this] val log = Logging(context.system, this)

    private def setImage(img: BufferedImage): Unit =
      Swing.onEDT {
        if (image != null) image.flush()
        image = img
        ggImageView.repaint()
      }

    def receive: Receive = {
      case Source.Control(_flags) => source ! Source.Control(_flags)
      case Source.GrayImage  (img) => setImage(img)
      case Source.ThreshImage(img) => setImage(img)
      case Source.LinesIn    (arr) =>
        Swing.onEDT {
          linesIn = arr
          ggImageView.repaint()
        }
      case Source.LinesExt   (arr) =>
        Swing.onEDT {
          linesExt = arr
          ggImageView.repaint()
        }

      case x =>
        log.warning(s"Control window received unknown message '$x'")
    }
  }

  private[this] val actor = system.actorOf(Props(new ActorImpl))

  private def updateFlags(): Unit = {
    val oldFlags = flags
    flags = 0
    if (ggGrayImage  .selected) flags |= Source.CtlGray
    if (ggThreshImage.selected) flags |= Source.CtlThresh
    if (ggLinesIn    .selected) flags |= Source.CtlLinesIn
    if (ggLinesExt   .selected) flags |= Source.CtlLinesExt
    if (flags != oldFlags) {
      actor ! Source.Control(flags)
    }
  }

  def open(): Unit = frame.open()
}