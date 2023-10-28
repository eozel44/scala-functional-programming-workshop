object s3_functional_datastructure{

  /**
     In the declaration trait List[+A], the + in front of the type parameter A is a variance annotation
     which signals that A is a covariant or 'positive' parameter of List. This means that, for instance,
     List[Dog] is considered a subtype of List[Animal], assuming Dog is a subtype of Animal.

     But notice now that Nil extends List[Nothing]. Nothing is a subtype of all types.
     Nil can be considered a List[Int], a List[Double], and so on
   */

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head:A,tail:List[A]) extends List[A]

  object List{
    /** variadic function that accepts zero or more arguments of type A */
    def apply[A](as:A*):List[A] ={
      if (as.isEmpty) Nil
      else Cons(as.head,apply(as.tail:_*)) /* no idea _* syntax */
    }

    def sum(l:List[Int]):Int =l match{
      case Nil => 0
      case Cons(head,tail) => head + sum(tail)
    }

    def sumTailRec(l:List[Int]):Int ={
      @annotation.tailrec
      def loop(l:List[Int], acc:Int):Int = l match{
        case Nil => acc
        case Cons(head,tail) => loop(tail, head+acc)
      }
      loop(l,0)
    }

    /** Recursion over lists and generalizing to higher-order functions */

    /**
    foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)((x,y) => x + y)
        1 + foldRight(Cons(2, Cons(3, Nil)), 0)((x,y) => x + y)
        1 + (2 + foldRight(Cons(3, Nil), 0)((x,y) => x + y))
        1 + (2 + (3 + (foldRight(Nil, 0)((x,y) => x + y))))
        1 + (2 + (3 + (0)))
        6
     */

    def foldRight[A,B](l:List[A], acc:B)(f:(A,B)=>B):B = l match {
      case Nil => acc
      case Cons(head, tail) => f(head,foldRight(tail,acc)(f))
    }

    def sumViaFoldRight(l:List[Int]) = foldRight(l,0)((a,b)=>a+b)

    /**
        foldRight is not tail-recursive and will StackOverflow for large lists.
        foldLeft that is tail-recursive
     */

    @annotation.tailrec
    def foldLeft[A,B](l:List[A], acc:B)(f:(B,A)=>B):B = l match{
      case Nil => acc
      case Cons(head,tail)=> foldLeft(tail,f(acc,head))(f)
    }

    def sumViaFoldLeft(l:List[Int]) = foldLeft(l,0)((a,b)=>a+b)

    def reverse[A](l:List[A]):List[A] = foldLeft(l,List[A]())((tail,head)=> Cons(head, tail))

    /** scala implementation of foldRight */

    def foldRightViaFoldLeft[A,B](l:List[A],acc:B)(f:(A,B)=>B) = foldLeft(reverse(l),acc)((a,b)=>f(b,a))

    /**
      map & filter functions
     */

    def map[A,B](l:List[A])(f:A=>B): List[B] = foldRightViaFoldLeft(l,List[B]())((head,tail)=>Cons(f(head),tail))
    def filter[A](l:List[A])(f:A=>Boolean):List[A] = foldRightViaFoldLeft(l,Nil:List[A])((head,tail) => if(f(head)) Cons(head,tail) else tail)




  }


  def main(args: Array[String]): Unit = {

    val ex = List(1,2,3,4,5)

    assert(List.sum(ex) == List.sumTailRec(ex))

    assert(List.sumTailRec(ex) == List.sumViaFoldRight(ex))

    assert(List.sumViaFoldRight(ex) == List.sumViaFoldRight(ex))

    assert(List.foldRightViaFoldLeft(ex,0)((x,y) => x +y) == List.sum(ex))

    assert(List.map(ex)(_ * 2) == List(2,4,6,8,10))

    assert(List.filter(ex)(a => a*2 == 5) == Nil)

    assert(List.filter(ex)(a => a*2 == 6) == List(3))

  }
}
