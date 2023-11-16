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
      println("hi"); 1 + 41
    })
    //result
    //hi
    //x: Int = 84

    assert(Stream(1, 2, 3).toList == List(1, 2, 3))
    assert(Stream(1, 2, 3).take(2).toList == List(1, 2))

  }
}

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


