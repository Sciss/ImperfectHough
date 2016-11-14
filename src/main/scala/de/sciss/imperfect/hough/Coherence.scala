/*
 *  Coherence.scala
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

object Coherence {
  final val permutations: Array[TriPerm] =
    Array(
      new TriPerm(0, 1, 2),
      new TriPerm(0, 2, 1),
      new TriPerm(1, 0, 2),
      new TriPerm(1, 2, 0),
      new TriPerm(2, 0, 1),
      new TriPerm(2, 1, 0)
    )
}
class Coherence(maxTri: Int) {
  import Coherence._

  /** Try align two sets of triangles.
    * Indices are `-1` for no match (triangle dies or is being born).
    * Otherwise, there lower 28 bits refer to the corresponding other array
    * (`triPrev` in `indicesNext` or `triNext` in `indicesPrev`),
    * and bits 29 to 31 contain the permutation index (into `Coherence.permutations`).
    * The highest bit is set if this triangle does not match any previous or successive
    * triangle.
    *
    * @param triPrev        previous set of triangles
    * @param numTriPrev     number of valid entries in `triPrev`
    * @param triNext        next set of triangles
    * @param numTriNext     number of valid entries in `triNext`
    * @param leftTol        maximum movement of triangle to the left   (positive value)
    * @param rightTol       maximum movement of triangle to the right  (positive value)
    * @param topTol         maximum movement of triangle to the top    (positive value)
    * @param bottomTol      maximum movement of triangle to the bottom (positive value)
    *
    * @return the number of matching triangles (for information purposes)
    */
  def run(triPrev: Array[Triangle], numTriPrev: Int, triNext: Array[Triangle], numTriNext: Int,
          leftTol: Int, rightTol: Int, topTol: Int, bottomTol: Int): Int = {

    var i = 0
    while (i < numTriPrev) {
      i += 1
      val tri = triPrev(i)
      tri.mkIncoherent()
    }

//    util.Arrays.fill(taken, 0, numTriPrev, false)

    var numMatches = 0

    // XXX TODO --- can we do better than quadratic?
    i = 0
    while (i < numTriNext) {
      val triN = triNext(i)
      triN.coh = -1
      var j = 0
      while (j < numTriPrev) {
        val triP = triPrev(j)
        if (triP.isIncoherent) {
          val perm = permutations
          var p    = 0
          while (p < 6) {
            val pi  = perm(p)
            val dx1 = triN.x1 - triP.x(pi._1)
            if (dx1 >= -leftTol && dx1 <= rightTol) {
              val dx2 = triN.x2 - triP.x(pi._2)
              if (dx2 >= -leftTol && dx2 <= rightTol) {
                val dx3 = triN.x3 - triP.x(pi._3)
                if (dx3 >= -leftTol && dx3 <= rightTol) {
                  val dy1 = triN.y1 - triP.y(pi._1)
                  if (dy1 >= -topTol && dy1 <= bottomTol) {
                    val dy2 = triN.y2 - triP.y(pi._2)
                    if (dy2 >= -topTol && dy2 <= bottomTol) {
                      val dy3 = triN.y3 - triP.y(pi._3)
                      if (dy3 >= -topTol && dy3 <= bottomTol) {
                        triP.mkCoherent()
                        triN.coh        = j | (p << 28)
//                        indicesPrev(j)  = i
//                        indicesNext(i)  = j | (p << 28)
                        // abort loops
                        p               = 6
                        j               = numTriPrev
                        numMatches     += 1
                      }
                    }
                  }
                }
              }
            }
            
            p += 1
          }
        }
        j += 1
      }

      i += 1
    }

    numMatches
  }
}
