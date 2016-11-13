/*
 *  MainLoop.scala
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

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object MainLoop {
  def run(system: ActorSystem, config: View.Config): ActorRef = {
    val sourceP = if (config.useGrabber) Source.live()
             else if (config.useIPCam  ) Source.ipCam(ip = config.cameraIP, password = config.cameraPassword,
                                                      hAngleStep = config.camHAngleStep, vAngle = config.camVAngle)
             else                        Source.files()

    val source  = system.actorOf(sourceP, "source")
    val loopP   = props(source)
    val loop    = system.actorOf(loopP, "test")
    loop ! Start
    source
  }

  case object Start
  case object Stop
  case class Analysis(lines: Array[LineI])

  def props(source: ActorRef): Props = Props(new MainLoop(source))
}
class MainLoop(source: ActorRef) extends Actor {
  import MainLoop._

  def receive: Receive = {
    case Start =>
      source ! Source.Open(1920, 1080)
      source ! Source.Task
//      source ! Source.Close

    case Analysis(lines) =>
      View.lines = lines
      source ! Source.Task

    case Stop =>
      source ! Source.Close
    // case _ =>
  }
}