package de.sciss.imperfect.hough

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import de.sciss.imperfect.hough.Analyze.Line

import scala.collection.immutable.{IndexedSeq => Vec}

object Test {
  def main(args: Array[String]): Unit = {
    val system  = ActorSystem("system")
    val sourceP = Source.props()
    val source  = system.actorOf(sourceP, "source")
    val testP   = props(source)
    val test    = system.actorOf(testP, "test")
    test ! Start
  }

  case object Start
  case object Stop
  case class Analysis(lines: Vec[Line])

  def props(source: ActorRef): Props = Props(new Test(source))
}
class Test(source: ActorRef) extends Actor {
  import Test._

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