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

    def flatMap[A,B](l:List[A])(f:A=>List[B]):List[B] = ???


    @annotation.tailrec
    def startsWith[A](l: List[A], sub: List[A]): Boolean = (l, sub) match {
      case (_,Nil) => true
      case (Cons(head,tail),Cons(headSub,tailSub))  if head == headSub =>  startsWith(tail, tailSub)
      case _ => false
    }

    @annotation.tailrec
    def hasSubsequence[A](l:List[A], sub:List[A]):Boolean =  l match {
      case Nil => sub==Nil
      case _ if startsWith(l,sub) => true
      case Cons(h,t) => hasSubsequence(t,sub)
    }

    /** other exercises */
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



  /**
     Algebraic data types can be used to define other data structures.
     Let's define a simple binary tree data structure
  */
  sealed trait Tree[+A] {
    def size: Int = this match {
      case Leaf(_) => 1
      case Branch(l,r) => 1 + l.size + r.size
    }

    def depth:Int = this match{
      case Leaf(_) => 1
      case Branch(l,r) => 1 + l.depth.max(r.depth)
    }

    def map[B](f:A=>B):Tree[B] = this match{
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(l.map(f), r.map(f))
    }

    def fold[B](f:A=>B)(g: (B,B) => B):B = this match{
      case Leaf(v) => f(v)
      case Branch(l,r) => g(l.fold(f)(g), r.fold(f)(g))
    }

    def sizeViaFold: Int = fold(a => 1)(1 + _ + _)
    def depthViaFold: Int = fold(a => 0)((d1,d2) => 1 + (d1 max d2))

    def mapViaFold[B](f: A => B): Tree[B] = ???

  }
  case class Leaf[A](value:A) extends Tree[A]
  case class Branch[A](left:Tree[A], right:Tree[A]) extends Tree[A]



  def main(args: Array[String]): Unit = {

    val ex = List(1,2,3,4,5)

    assert(List.sum(ex) == List.sumTailRec(ex))

    assert(List.sumTailRec(ex) == List.sumViaFoldRight(ex))

    assert(List.sumViaFoldRight(ex) == List.sumViaFoldRight(ex))

    assert(List.foldRightViaFoldLeft(ex,0)((x,y) => x +y) == List.sum(ex))

    assert(List.map(ex)(_ * 2) == List(2,4,6,8,10))

    assert(List.filter(ex)(a => a*2 == 5) == Nil)

    assert(List.filter(ex)(a => a*2 == 6) == List(3))

    assert(List.hasSubsequence(ex,List(1,2,3)) == true)

  }
}
