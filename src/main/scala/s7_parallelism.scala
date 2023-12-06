package parallelism

object s7_parallelism {

  /**
     This implementation isn't the usual left fold, as.foldLeft(0)(_ + _);
     instead we are using a "divide and conquer" algorithm. We divide the sequence
     in half using the splitAt function, recursively sum both halves, then combine their results.
     And unlike the foldLeft-based implementation,
     this implementation can be parallelized—the two halves can be summed in parallel.
   */
  def sum(as:IndexedSeq[Int]):Int =
    if(as.size<=1) as.headOption getOrElse 0
    else {
      val (l,r) = as.splitAt(as.length/2)
      sum(l) + sum(r)
    }
  def main(args: Array[String]): Unit = {

    val ls = IndexedSeq(1,2,3,4,5,6,7,8,9)
    assert(sum(ls)==45)

  }
}