package com.madgag.algo.sorting.kwaymerge

import com.madgag.algo.sorting.kwaymerge.Merge.{leafLineLengthRequiredFor, mergeIterable}
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

  it should "work for many different things" in {
    forAll { (n: Seq[Seq[Int]]) =>
      mergeIterable(n.map(_.sorted) *).toSeq shouldBe sorted
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
