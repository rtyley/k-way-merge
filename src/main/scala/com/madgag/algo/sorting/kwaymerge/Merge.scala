package com.madgag.algo.sorting.kwaymerge

import scala.math.BigInt.int2bigInt

object Merge {
  def arrayLengthRequiredFor(k: Int): Int = (1 << (k+1).bitLength) - 1

  def mergeIterable[T: Ordering](seqs: Iterable[T]*): Iterable[T] = {
    ???
  }

  def merge[T: Ordering](seqs: Iterator[T]*): Iterator[T] = {

//    val tree: Array[Option[(T, Iterator[T])]] = for {
//
//    }
//
//    new Iterator[T] {
//      override def hasNext: Boolean = tree(0).isDefined
//
//      override def next(): T = {
//        val n = tree(0)
//
//        ???
//      }
//    }
    ???
  }
}
