/*
 *  ImageDecodeTest.scala
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

import java.io.ByteArrayOutputStream

import de.sciss.file._
import org.bytedeco.javacpp.indexer.UByteRawIndexer
import org.bytedeco.javacpp.opencv_core.Mat
import org.bytedeco.javacpp.{opencv_core, opencv_imgcodecs}

object ImageDecodeTest extends App {
  val password  = args.headOption.getOrElse(sys.error("Must specify camera password"))

//  val fIn   = userHome / "Documents" / "temp" / "amcrest_test" / "image2.jpg"
  val fOut  = userHome / "Documents" / "temp" / "amcrest_test" / "image2-out.jpg"

//  val fis   = new FileInputStream(fIn)
//  val sz    = fis.available()
//  var arr   = new Array[Byte](sz)
//  fis.read(arr)
//  fis.close()

  val cmd = Seq("wget", "http://192.168.0.41/cgi-bin/snapshot.cgi",
    "--user", "admin", "--password", password, "-O-", "-q")
//  println(cmd.mkString(" "))

  import sys.process._
  val baos  = new ByteArrayOutputStream(640 * 1024) // init capacity of 640K should be fine for basically all images
  val res   = Process(cmd).#>(baos).!
//  println(s"result: $res")
  require(res == 0, s"wget exited with error code $res")
  val arr   = baos.toByteArray
  val sz    = arr.length
  println(s"array size: $sz")

  val matRaw  = new Mat(1, sz, opencv_core.CV_8UC1)
  val idxRaw  = matRaw.createIndexer[UByteRawIndexer]()
  var i = 0
  while (i < sz) {
    // XXX TODO --- this can't be the most efficient way???
    idxRaw.put(0, i, arr(i))
    i += 1
  }
  val matIn = opencv_imgcodecs.imdecode(matRaw, opencv_imgcodecs.IMREAD_COLOR)
  opencv_imgcodecs.imwrite(fOut.path, matIn)
}