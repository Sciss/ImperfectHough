/*
 *  SpaceCommunicator.scala
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
import de.sciss.imperfect.hough.SpaceCommunicator.Position
import de.sciss.numbers.IntFunctions
import de.sciss.osc
import de.sciss.osc.UDP
import de.sciss.osc.Implicits._

object SpaceCommunicator {
  def apply(system: ActorSystem, config: View.Config): ActorRef = {
    val target  = config.rattleIP -> config.rattlePort
    val t       = UDP.Transmitter(target)
    if (config.rattleDump) t.dump()
    t.connect()
    log("rattle OSC set-up.")
    val spaceP  = Props(new SpaceCommunicator(t, pad = 0 /* config.rattlePad */))
    val space   = system.actorOf(spaceP, "osc")
    space
  }

  final case class Position(x1: Int, x2: Int)
}
final class SpaceCommunicator(t: osc.Transmitter.Directed, pad: Int) extends Actor {
  override def receive: Receive = {
    case Position(x1, x2) =>
      val x1m   = IntFunctions.wrap(x1 - pad, 0, VisibleWidth)
      val x2m   = IntFunctions.wrap(x2 + pad, 0, VisibleWidth)
      val x1f   = x1m.toFloat
      val x2f   = (if (x2m >= x1m) x2m else x2m + VisibleWidth).toFloat
//      val x1f = 0f
//      val x2f = 1920f
      t ! osc.Message("/recImp", x1f, x2f, 1f)
  }
}