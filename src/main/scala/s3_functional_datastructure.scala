object s3_functional_datastructure{


  /**
     In the declaration trait List[+A], the + in front of the type parameter A is a variance annotation
     which signals that A is a covariant or 'positive' parameter of List. This means that, for instance,
     List[Dog] is considered a subtype of List[Animal], assuming Dog is a subtype of Animal.

     But notice now that Nil extenl List[Nothing]. Nothing is a subtype of all types.
     Nil can be considered a List[Int], a List[Double], and so on
   */

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head:A, tail: List[A]) extends List[A]

  object List{
    def sum(ints: List[Int]):Int={
      ints match{
        case Nil => 0
        case Cons(head,tail) => head + sum(tail)
      }
    }
    def product(l: List[Double]):Double = l match{
      case Nil => 1.0
      case Cons(0.0,_) => 0.0
      case Cons(head, tail) => head * product(tail)
    }

    def apply[A](as: A*):List[A] ={ /** variadic function that accepts zero or more arguments of type A */
      if(as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }

    /** exercises */
    def remove[A](l:List[A], item: A):List[A]= l match{
      case Nil => Nil
      case Cons(head,tail) => if(head==item) tail else l
    }

    def setHead[A](item: A, l:List[A]):List[A]= l match{
      case Nil => Cons(item,Nil)
      case Cons(head,tail) =>  Cons(item,tail)
    }

    def drop[A](l: List[A], n: Int): List[A] = if(n<=0) l else l match{
      case Nil => Nil
      case Cons(head, tail) => drop(tail,n-1)
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Cons(head, tail) => if (f(head)) tail else dropWhile(tail, f)
      case _ => l
    }

    def append[A](l1:List[A], l2:List[A]):List[A] = l1 match {
      case Nil => l2
      case Cons(head, tail) => Cons(head,append(tail,l2))
    }

    def init[A](l: List[A]): List[A]  = l match{
      case Nil => Nil
      case Cons(_,Nil) => Nil
      case Cons(head,tail) => Cons(head, init(tail))
    }
  }

  def main(args: Array[String]): Unit = {
    val ex1: List[Double] = Nil
    val ex2: List[Int] = Cons(1, Nil)
    val ex3: List[String] = Cons("a", Cons("b",Nil))

    val ex4 = List(1,2,3,4)

    assert(List.remove(ex4,1) == List(2,3,4))
    assert(List.drop(ex4, 2) == List(3,4))
    assert(List.dropWhile(ex4,(x:Int) => x % 3 == 0) == List(4))
    assert(List.init(ex4) == List(1,2,3))









  }
}
