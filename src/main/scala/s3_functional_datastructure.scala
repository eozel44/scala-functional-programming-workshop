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
  case class Cons[+A](head:A, tail: List[A]) extends List[A]

  object List{
    def sum(ints: List[Int]):Int={
      ints match{
        case Nil => 0
        case Cons(head,tail) => head + sum(tail)
      }
    }
    def product(ds: List[Double]):Double = ds match{
      case Nil => 1.0
      case Cons(0.0,_) => 0.0
      case Cons(head, tail) => head * product(tail)
    }

    def apply[A](as: A*):List[A] ={ /** variadic function that accepts zero or more arguments of type A */
      if(as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }

  }

  def main(args: Array[String]): Unit = {
    val ex1: List[Double] = Nil
    val ex2: List[Int] = Cons(1, Nil)
    val ex3: List[String] = Cons("a", Cons("b",Nil))









  }
}
