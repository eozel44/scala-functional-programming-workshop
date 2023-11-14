package laziness

object s5_laziness{

  sealed abstract class Stream[+A]{

    def toList: List[A] = {
      @annotation.tailrec
      def loop(s:Stream[A], acc:List[A]):List[A] = s match{
        case Cons(h, t) => loop(t(), h()::acc)
        case Empty => acc.reverse
      }
      loop(this, Nil)
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


  def main(args: Array[String]): Unit = {


    assert(Stream(1,2,3).toList == List(1,2,3))

  }
}
