package com.madgag.algo.sorting.kwaymerge

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MergeTest extends AnyFlatSpec with should.Matchers {
  "Merge" should "merge several sorted lists and merge them into a single sorted list" in {
    // Example from https://en.wikipedia.org/wiki/K-way_merge_algorithm#Merge
    Merge.mergeIterable(
      Seq(2, 7, 16),
      Seq(5, 10, 20),
      Seq(3, 6, 21),
      Seq(4, 8, 9)
    ) shouldBe Seq(2, 3, 4, 5, 6, 7 , 8, 9, 10, 16, 20, 21)
  }

  it should "use an array of appro size" in {
    Merge.arrayLengthRequiredFor(2) shouldBe 3
    Merge.arrayLengthRequiredFor(3) shouldBe 7
    Merge.arrayLengthRequiredFor(4) shouldBe 7
  }
}
