package com.madgag.algo.sorting.kwaymerge

import com.madgag.algo.sorting.kwaymerge.Merge.leafLineLengthRequiredFor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MergeTest extends AnyFlatSpec with should.Matchers {
  "Merge" should "merge several sorted lists and merge them into a single sorted list" in {
    // Example from https://en.wikipedia.org/wiki/K-way_merge_algorithm#Merge
    Merge.merge(
      Seq(2, 7, 16).iterator,
      Seq(5, 10, 20).iterator,
      Seq(3, 6, 21).iterator,
      Seq(4, 8, 9).iterator
    ).toSeq shouldBe Seq(2, 3, 4, 5, 6, 7 , 8, 9, 10, 16, 20, 21)
  }

  it should "work dammit" in {
    Merge.merge(
      Seq('h', 's').iterator,
      Seq('a', 'r').iterator,
      Seq('b', 'o').iterator
    ).toSeq.mkString shouldBe "abhors"
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
