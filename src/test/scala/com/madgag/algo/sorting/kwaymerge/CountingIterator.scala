package com.madgag.algo.sorting.kwaymerge

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.AbstractIterator

object CountingIterator {
  def apply[T](items: T*): CountingIterator[T] = new CountingIterator[T](items.iterator)
}

class CountingIterator[T](wrapped: Iterator[T]) extends AbstractIterator[T] {

  private val nextCallCount = new AtomicInteger()
  private val hasNextCallCount = new AtomicInteger()

  override def hasNext: Boolean = {
    hasNextCallCount.getAndIncrement()
    wrapped.hasNext
  }

  override def next(): T = {
    nextCallCount.getAndIncrement()
    wrapped.next()
  }

  def nextCalls: Int = nextCallCount.get()

  def hasNextCalls: Int = hasNextCallCount.get()
}