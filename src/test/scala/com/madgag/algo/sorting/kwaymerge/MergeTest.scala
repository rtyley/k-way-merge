package com.madgag.algo.sorting.kwaymerge

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MergeTest extends AnyFlatSpec with should.Matchers {
  "Merge" should "merge several sorted lists and merge them into a single sorted list" in {
    // Example from https://en.wikipedia.org/wiki/K-way_merge_algorithm#Merge
    Merge.merge(
      Seq(2, 7, 16),
      Seq(5, 10, 20),
      Seq(3, 6, 21),
      Seq(4, 8, 9)
    ) shouldBe Seq(2, 3, 4, 5, 6, 7 , 8, 9, 10, 16, 20, 21)
  }
}
