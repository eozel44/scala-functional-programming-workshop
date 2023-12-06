package parallelism

import java.util.concurrent._
import language.implicitConversions

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

  object Par {
    type Par[A] = ExecutorService => Future[A]

    /**
       unit => injects a constant into a parallel computation.
       map2 => combines the results of two parallel computations with a binary function.
       fork => spawns a parallel computation. The computation will not be spawned until forced by run.
       run  => extracts a value from a Par by actually performing the computation.
     */
    def run[A](s:ExecutorService)(a:Par[A] ):Future[A] = a(s)
    def unit[A](a:A):Par[A] = ???
    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = ???
    def fork[A](a: => Par[A]): Par[A] = ???
    def async[A](a: => A): Par[A] = fork(unit(a))

  }

  def main(args: Array[String]): Unit = {

    val ls = IndexedSeq(1,2,3,4,5,6,7,8,9)
    assert(sum(ls)==45)

  }
}