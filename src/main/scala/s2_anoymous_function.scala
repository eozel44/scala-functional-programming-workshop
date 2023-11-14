package anoymousFunction
import scala.annotation.tailrec

object s2_anoymous_function{

  def main(args: Array[String]): Unit = {

    /** Abstracting over the type */

    def findFirst(ds:Array[Double], key:Double):Int={
        @annotation.tailrec
        def loop(n:Int):Int ={
         if(n>= ds.length) -1
         else if(ds(n) == key) n
         else loop(n+1)
        }
        loop(0)
      }

    /** Very often, we want to write code which works for any type it is given. */
    assert(findFirst(Array(1d,2d,6d,7d,3d,6d), 6d) == 2)
    assert(findFirst(Array(1d,2d,6d,7d,3d,6d), 7d) == 3)

    def findFirstAbstract[A](ds:Array[A], p:A=>Boolean):Int={
      @annotation.tailrec
      def loop(n:Int):Int={
        if(n>=ds.length) -1
        else if(p(ds(n))) n
        else loop(n+1)
      }
      loop(0)
    }

    assert(findFirstAbstract(Array(1d,2d,6d,7d,3d,6d), (k:Double) => k==6d) ==2)
    assert(findFirstAbstract(Array("a","b","d","c"), (k:String) => k.equals("c")) ==3)

    def isSorted[A](ds:Array[A],gt: (A,A) => Boolean):Boolean ={
      def loop(n:Int):Boolean ={
        if(n>=ds.length-1) true
        else if (gt(ds(n), ds(n+1))) loop(n+1)
        else false
      }
      loop(0)
    }

    assert(isSorted(Array(1d,2d,6d,7d,3d,6d), (x:Double, y:Double) =>x<y) == false)
    assert(isSorted(Array(1d,2d,3d,4d,5d,7d), (x:Double, y:Double) =>x<y) == true)
    assert(isSorted(Array(1,2,3,4,6,5), (x:Int, y:Int) =>x<y) == false)

    /** Functions are ordinary objects
     * When we define a function literal, what is actually being defined is an object with a method called apply.
     * Scala has a special rule for this method name, so that objects that
     * have an apply method can be called as if they were themselves methods.
     * When we define a function literal like (a, b) => a < b this is really syntax sugar for
       object creation:
    */
      val lessThan = new Function2[Int, Int, Boolean] {
          def apply(a: Int, b: Int) = a < b
      }
    assert(isSorted(Array(1,2,3,4,6,5), lessThan) == false)

    /**
     * Function2 is trait has a single method called apply and when we call the lessThan function with lessThan(10, 20),
     * it is really syntax sugar for calling its apply method:
     *
         lessThan.apply(10, 20)
     *
     * Also provided are Function1, Function3, and others, taking a number of arguments indicated by the name.
     */


    /** Polymorphic functions are often so constrained by their type
     that they only have one implementation! Here's an example:
    */

    def partial1[A,B,C](a: A, f: (A, B) => C): B => C =
      b => f(a, b)

    // write the return type as 'A => B => C'
    def curry[A,B,C](f: (A, B) => C): A => (B => C) =
      a => b => f(a,b)

    //Implement 'uncurry'
    def uncurry[A,B,C](f: A => B => C): (A, B) => C =
      (a,b) => f(a)(b)

    //Implement 'compose'
    def compose[A,B,C](f: B => C, g: A => B): A => C =
      a => f(g(a))


    /**
     * All that the theory of currying means is that a function that takes multiple arguments
     * can be translated into a series of function calls that each take a single argument.
     *
     * result = f(x)(y)(z)
     * f1 = f(x)
       f2 = f1(y)
       result = f2(z)
     * */

    def add(x: Int, y: Int):Int = x + y

    assert((add _).isInstanceOf[Function2[_, _, _]] == true)

    val curryAdd: Int => Int => Int = (l) => (r) => add(l, r)

    val addTwo = curryAdd(2)
    assert(addTwo(2)==add(2,2))
    assert(addTwo(20)==add(2,20))



  }
}
