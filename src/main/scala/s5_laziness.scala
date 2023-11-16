package laziness

import Stream._


object s5_laziness{

  def main(args: Array[String]): Unit = {

    def maybeTwice(b: Boolean, i: => Int) = if (b) i + i else 0

    val x = maybeTwice(true, {
      println("hi"); 1 + 41
    })
    //result
    //hi
    //hi
    //x: Int = 84

    /**
     *  Adding the lazy keyword to a val declaration will cause Scala to delay
     *  evaluation of the right hand side of that lazy val declaration until it is first referenced.
     *  It will also cache the result so that subsequent references to it don't trigger repeated evaluation.
     */
    def maybeTwiceLazy(b: Boolean, i: => Int) = {
      lazy val j = i
      if (b) j + j else 0
    }

    val xx = maybeTwiceLazy(true, {
      println("hello"); 1 + 41
    })
    //result
    //hi
    //x: Int = 84

    assert(Stream(1, 2, 3).toList == List(1,2,3))
    assert(Stream(1, 2, 3).take(2).toList == List(1, 2))
    assert(Stream(1, 2, 3).takeWhile( l=> l<3).toList == List(1, 2))
    assert(Stream(1, 2, 3).exists(_ == 2) == true)
    assert(Stream(1, 2, 3).exists(_ == 5) == false)

  }
}
/** We'll see how chains of transformations on streams are fused into a single pass, through the use of laziness */
sealed trait Stream[+A]{

  def toList: List[A] = {
    @annotation.tailrec
    def loop(s:Stream[A], acc:List[A]):List[A] = s match{
      case Cons(h, t) => loop(t(), h()::acc)
      case Empty => acc.reverse
    }
    loop(this, Nil)
  }

  def take(n: Int): Stream[A] = this match {
    case _ if n <= 0 => Empty
    case Cons(h, t)  => cons(h(), t().take(n - 1))
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t)  if f(h()) => cons(h(), t().takeWhile(f))
    case _ => Empty
  }

/** explicit recursion

 def exists(f:A => Boolean):Boolean = this match {
    case Cons(h, t) => f(h()) || t().exists(f)
    case _ => false
  }

*/

/** The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    If `f` doesn't evaluate its second argument, the recursion never occurs.
 */
def foldRight[B](acc: => B)(f: (A, => B) => B):B = this match{
  case Cons(h,t) => f(h(), t().foldRight(acc)(f))
  case _ => acc
}

/** Here b is the unevaluated recursive step that folds the tail of the stream.
    If f(a) returns true, b will never be evaluated and the computation terminates early.
*/
  def exists(f:A=>Boolean):Boolean =
      foldRight(false)((a, b) => f(a) || b)


}

object Empty extends Stream[Nothing]
case class Cons[+A](head:() => A, tail:() => Stream[A]) extends Stream[A]

object Stream{
  def empty[A]:Stream[A] = Empty

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] ={
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A](as: A*):Stream[A]= if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}


