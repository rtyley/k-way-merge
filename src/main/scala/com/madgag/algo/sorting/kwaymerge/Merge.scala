package com.madgag.algo.sorting.kwaymerge

import java.lang.Integer.highestOneBit
import scala.annotation.tailrec
import scala.collection.AbstractIterable
import scala.math.Ordered.orderingToOrdered

object Merge {

  def leafLineLengthRequiredFor(k: Int): Int = highestOneBit(k - 1) << 1

  def mergeIterable[T: Ordering](seqs: Iterable[T]*): Iterable[T] = new AbstractIterable[T] {
    override def iterator: Iterator[T] = merge(seqs.map(_.iterator)*)
  }

  def merge[T](seqs: Iterator[T]*)(using ordering: Ordering[T]): Iterator[T] = if (seqs.isEmpty) Iterator.empty
    else if (seqs.size == 1) seqs.head else {
      type NodeValue = Option[NodePayload[T]]

//      given Ordering[NodeValue] = {
//        case (Some(x), Some(y)) => ordering.compare(x._1, y._1)
//        case (None, None) => 0
//        case (None, _) => 1
//        case (_, None) => -1
//      }

      given Ordering[NodeValue] = new Ordering[NodeValue] {
        override def compare(x: NodeValue, y: NodeValue): Int = if (x.isDefined && y.isDefined) {
          ordering.compare(x.get._1, y.get._1)
        } else if (x.isEmpty && y.isEmpty) 0 else if (x.isEmpty) 1 else -1
      }

      val leafLineLength = leafLineLengthRequiredFor(k = seqs.size)
      val leafLineBaseIndex = leafLineLength - 1

      val tree = Array.ofDim[NodeValue](leafLineBaseIndex + leafLineLength)

      def leafNodeValueForNextFrom(seq: Iterator[T], seqIndex: Int): NodeValue =
        seq.nextOption().map(value => NodePayload(value, seqIndex))

      def compareAtTreeIndex(ti: Int, a: NodeValue, b: NodeValue): NodeValue = {
        if (a < b)
          tree(ti) = b ; a
        else
          tree(ti) = a ; b
      }

      def parentOf(treeIndex: Int) = (treeIndex - 1) / 2

      def initialiseAndDetermineInitialWinner(): NodeValue = {
        for ((seq, seqIndex) <- seqs.zipWithIndex) {
          tree(leafLineBaseIndex + seqIndex) = leafNodeValueForNextFrom(seq, seqIndex)
        }
        for (ti <- leafLineBaseIndex + seqs.size until tree.length) {
          tree(ti) = None
        }

        def determineWinnerOf(ti: Int): NodeValue = if (ti >= leafLineBaseIndex) tree(ti) else {
          val lhcIndex = (ti * 2) + 1
          compareAtTreeIndex(ti, determineWinnerOf(lhcIndex), determineWinnerOf(lhcIndex + 1))
        }

        determineWinnerOf(0)
      }

      @tailrec def replayUpFrom(changedChildTreeIndex: Int, newWinnerSentUpFromChild: NodeValue): NodeValue = {
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
          case Uninitialized => initialiseAndDetermineInitialWinner()
          case PreviousWinner(seqIndex) => replayGamesFor(seqIndex)
        }
        winner.map(np => np.value -> PreviousWinner(np.seqIndex))
      }
    }

  case class NodePayload[T](value: T, seqIndex: Int)

  sealed trait State
  case object Uninitialized extends State
  case class PreviousWinner(seqIndex: Int) extends State
}
