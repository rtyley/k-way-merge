package com.madgag.algo.sorting.kwaymerge

import com.madgag.algo.sorting.kwaymerge.Merge.{leafLineLengthRequiredFor, merge, mergeIterable}
import org.scalatest.Inspectors
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.language.postfixOps

class MergeTest extends AnyFlatSpec with should.Matchers with ScalaCheckPropertyChecks {
  "Merge" should "merge several sorted lists and merge them into a single sorted list" in {
    // Example from https://en.wikipedia.org/wiki/K-way_merge_algorithm#Merge
    mergeIterable(
      Seq(2, 7, 16),
      Seq(5, 10, 20),
      Seq(3, 6, 21),
      Seq(4, 8, 9)
    ).toSeq shouldBe Seq(2, 3, 4, 5, 6, 7 , 8, 9, 10, 16, 20, 21)
  }

  it should "work for anything with an Ordering, not just numbers" in {
    mergeIterable(
      Seq('h', 's'),
      Seq('a', 'r'),
      Seq('b', 'o')
    ).mkString shouldBe "abhors"
  }

  it should "work for all ScalaCheck testcases" in forAll { (seqs: Seq[Seq[Int]]) =>
    val mergedSeq = mergeIterable(seqs.map(_.sorted) *).toSeq
    mergedSeq shouldBe sorted
    mergedSeq shouldBe seqs.flatten.sorted
  }

  it should "do minimal work" in forAll { (seqs: Seq[Seq[Int]]) =>
    val countingIterators: Seq[CountingIterator[Int]] = seqs.map(seq => CountingIterator(seq.sorted*))
    val mergedIterator = merge(countingIterators *)

    Inspectors.forAll(countingIterators) { countingIterator =>
      countingIterator.hasNextCalls shouldBe 0
      countingIterator.nextCalls shouldBe 0
    }

    whenever(seqs.size > 1) {
      val expectedSequence = seqs.flatten.sorted

      val isNonEmpty = mergedIterator.hasNext // this will force initialisation of the loser-tree

      Inspectors.forAll(countingIterators) { countingIterator =>
        countingIterator.hasNextCalls shouldBe 1
        countingIterator.nextCalls should be <= 1 // empty sequences don't get 'next' called
      }

      for {
        (value, index) <- mergedIterator.zipWithIndex
      } {
        value shouldBe expectedSequence(index)
        countingIterators.map(_.hasNextCalls).sum shouldBe (seqs.size + index)
        countingIterators.map(_.nextCalls).sum shouldBe <= (seqs.size + index)
      }
    }
  }


  it should "use a leaf line of appro size" in {
    leafLineLengthRequiredFor(2) shouldBe 2
    leafLineLengthRequiredFor(3) shouldBe 4
    leafLineLengthRequiredFor(4) shouldBe 4
    leafLineLengthRequiredFor(5) shouldBe 8
    leafLineLengthRequiredFor(8) shouldBe 8
    leafLineLengthRequiredFor(9) shouldBe 16
  }
}
