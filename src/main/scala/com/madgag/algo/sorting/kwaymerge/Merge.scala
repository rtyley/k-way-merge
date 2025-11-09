package com.madgag.algo.sorting.kwaymerge

import java.lang.Integer.highestOneBit
import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered

object Merge {

  def leafLineLengthRequiredFor(k: Int): Int = highestOneBit(k - 1) << 1

  def mergeIterable[T: Ordering](seqs: Iterable[T]*): Iterable[T] = {
    ???
  }

  def merge[T](seqs: Iterator[T]*)(using ordering: Ordering[T]): Iterator[T] = {
    case class NodePayload(value: T, seqIndex: Int)

    given Ordering[Option[NodePayload]] =
      (x: Option[NodePayload], y: Option[NodePayload]) =>
        (x, y) match {
          case (Some(x), Some(y)) => ordering.compare(x._1, y._1)
          case (None, None) => 0
          case (None, _) => 1
          case (_, None) => -1
        }

    val leafLineLength = leafLineLengthRequiredFor(k = seqs.size)
    val leafLineBaseIndex = leafLineLength - 1

    val tree = Array.ofDim[Option[NodePayload]](leafLineBaseIndex + leafLineLength)

    def leafNodeValueForNextFrom(seq: Iterator[T], seqIndex: Int): Option[NodePayload] =
      seq.nextOption().map(value => NodePayload(value, seqIndex))

    for ((seq, seqIndex) <- seqs.zipWithIndex) {
      tree(leafLineBaseIndex + seqIndex) = leafNodeValueForNextFrom(seq, seqIndex)
    }
    for (ti <- leafLineBaseIndex + seqs.size until tree.length) {
      tree(ti) = None
    }

    def compareAtTreeIndex(ti: Int, a: Option[NodePayload], b: Option[NodePayload]): Option[NodePayload] = {
      if (a < b)
        tree(ti) = b ; a
      else
        tree(ti) = a ; b
    }

    def parentOf(treeIndex: Int) = (treeIndex - 1) / 2

    def determineWinnerOf(ti: Int): Option[NodePayload] = if (ti >= leafLineBaseIndex) tree(ti) else {
      val lhcIndex = (ti * 2) + 1
      compareAtTreeIndex(ti, determineWinnerOf(lhcIndex), determineWinnerOf(lhcIndex + 1))
    }

    @tailrec def replayUpFrom(changedChildTreeIndex: Int, newWinnerSentUpFromChild: Option[NodePayload]): Option[NodePayload] = {
      val ti = parentOf(changedChildTreeIndex)
      val winner = compareAtTreeIndex(ti, tree(ti), newWinnerSentUpFromChild)
      if (ti == 0) winner else replayUpFrom(ti, winner)
    }

    def replayGamesFor(seqIndex: Int) = {
      val ti = leafLineBaseIndex + seqIndex
      val leafValue = leafNodeValueForNextFrom(seqs(seqIndex), seqIndex)
      tree(ti) = leafValue
      replayUpFrom(ti, leafValue)
    }

    Iterator.unfold[T, State](Uninitialized) { state =>
      val winner = state match {
        case Uninitialized => determineWinnerOf(0)
        case PreviousWinner(seqIndex) => replayGamesFor(seqIndex)
      }
      winner.map(np => np.value -> PreviousWinner(np.seqIndex))
    }
  }
}

sealed trait State

case object Uninitialized extends State

case class PreviousWinner(seqIndex: Int) extends State