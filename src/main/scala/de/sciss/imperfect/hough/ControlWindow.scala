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

  private[this] var image: BufferedImage = _

  private[this] val ggImageView = new Component {
    private[this] val colrGreen = new Color(0, 0x80, 0)

    opaque        = true
    preferredSize = new Dimension(1920/2, 1080/2)

    override protected def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      val w = peer.getWidth
      val h = peer.getHeight
      if (image == null) {
        g.setColor(colrGreen)
        g.fillRect(0, 0, w, h)
      } else {
        g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
        g.drawImage(image, 0, 0, w, h, null)
      }
    }
  }

  private[this] val frame: Frame = new Frame(gc.orNull) {
    title = "Hough Control"
    contents = new BorderPanel {
      add(new FlowPanel(ggGrayImage, ggThreshImage), BorderPanel.Position.North )
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
    if (flags != oldFlags) {
      actor ! Source.Control(flags)
    }
  }

  def open(): Unit = frame.open()
}