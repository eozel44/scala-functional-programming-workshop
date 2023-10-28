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

    /** Recursion over lists and generalizing to higher-order functions */

    def foldRight[A,B](l:List[A], acc:B)(f:(A,B)=>B): B = l match {
      case Nil => acc
      case Cons(head, tail) => f(head, foldRight(tail,acc)(f))
    }
    def sum2(l:List[Int]) = foldRight(l, 0)((x:Int,y:Int) => x+y)

    /**
        foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)((x,y) => x + y)
        1 + foldRight(Cons(2, Cons(3, Nil)), 0)((x,y) => x + y)
        1 + (2 + foldRight(Cons(3, Nil), 0)((x,y) => x + y))
        1 + (2 + (3 + (foldRight(Nil, 0)((x,y) => x + y))))
        1 + (2 + (3 + (0)))
        6
     */
    def sum3(l:List[Long]) = foldRight(l, 0L)((x,y) => x+y)
    def product2(l:List[Double]) = foldRight(l, 1.0)(_ * _)  /** The anonymous function (x,y) => x * y can be written as _ * _ */
    def length[A](l: List[A]): Int = foldRight(l,0)((_,acc) => acc + 1)

    /**
       foldRight is not tail-recursive and will StackOverflow for large lists.
       foldLeft that is tail-recursive
     */
    @annotation.tailrec
    def foldLeft[A,B](l: List[A], acc: B)(f: (B,A) => B): B = l match {
         case Nil => acc
         case Cons(head, tail) =>  foldLeft(tail,f(acc,head))(f)
    }
    def sumViaFoldLeft(l:List[Int]):Int = foldLeft(l,0) ((x:Int, y:Int) => x+y)
    def productViaFoldLeft(l:List[Double]) = foldLeft(l,1.0) ((x:Double, y:Double) => x*y)
    def lengthViaFoldLeft[A](l:List[A]):Int = foldLeft(l,0) ((acc:Int, h:A) => acc + 1)
    def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h,acc))

    /** scala implementation of foldRight */
    def foldRightViaFoldLeft[A,B](l:List[A], acc:B)(f:(A,B)=>B):B = foldLeft(reverse(l),acc)((b, a) => f(a, b))

    /**
     * map function
     * */

    def map[A,B](l: List[A])(f: A => B): List[B] = foldRightViaFoldLeft(l,List[B]())((h,t)=>Cons(f(h),t))
    def filter[A](l:List[A], f:A=>Boolean):List[A] = foldRightViaFoldLeft(l,Nil:List[A])((h,t)=> if (f(h)) Cons(h,t) else t)

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
    assert(List.sum2(ex4) == 10)
    assert(List.sum2(ex4) == List.foldRight(ex4,0)((x,y) => x +y))
    assert(List.length(ex4) == 4)
    assert(List.length(ex3) == 2)

    assert(List.sum2(ex4) == List.sumViaFoldLeft(ex4))
    assert(List.length(ex4) == List.lengthViaFoldLeft(ex4))
    assert(List.foldRightViaFoldLeft(ex4,0)((x,y) => x +y) == List.sum2(ex4))










  }
}
