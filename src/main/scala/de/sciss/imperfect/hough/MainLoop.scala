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
import de.sciss.imperfect.hough.Source.Analysis

object MainLoop {
  def apply(system: ActorSystem, source: ActorRef, config: View.Config): ActorRef = {
    val loopP   = props(source)
    val loop    = system.actorOf(loopP, "test")
    loop
  }

  final case class Start(view: View)
  case object Stop

  def props(source: ActorRef): Props = Props(new MainLoop(source))
}
final class MainLoop(val source: ActorRef) extends Actor {
  import MainLoop._

  private[this] var view: View = _

  def receive: Receive = {
    case a: Analysis => view.update(a)
//      source ! Source.Task

      // forward this one:
    case Source.Task => source ! Source.Task

    case Start(_view) =>
      view = _view
      source ! Source.Open(1920, 1080)
      source ! Source.Task
    //      source ! Source.Close

    case Stop =>
      source ! Source.Close
    // case _ =>
  }
}