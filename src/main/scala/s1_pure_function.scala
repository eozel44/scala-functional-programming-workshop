package pureFunction
import scala.annotation.tailrec

/** Pure Function
 *
 * A function, f, with input type A and output type B (as a single type: A => B, pronounced "A to B" or "A arrow B")
 * is a computation which relates every value a of type A to exactly
 * one value b of type B such that b is determined solely by the value of a.
 * Any changing state of an internal or external process is irrelevant to computing the result f(a).
 * In other words, a function has no observable effect on the execution of the
 * program other than to compute a result given its inputs; we say that it has no side-effects.
 *
 * There are lots of pure functions which you are already familiar with.
 * Consider the addition (+) function on integers. It takes two integer values and returns an integer value.
 * For any two given integer values it will always return the same integer value.
 *
 * This all means for an expression to be referentially transparent—in any program,
 * the expression can be replaced by its result without changing the meaning of the program.
 *
 */
object s1_pure_function{

  def main(args: Array[String]): Unit = {

    val x = "Hello, World"
    val r1 = x.reverse
    val r2 = x.reverse

    /** reverse function is referentially transparent**/
    assert(r1==r2)
    assert(r1=="Hello, World".reverse)
    assert(r2=="Hello, World".reverse)

    val y = new StringBuilder("Hello")
    val z = y.append(", World")

    val r3 = z.toString
    val r4 = z.toString

    /** append function is NOT referentially transparent */
    assert(r3==r4)
    assert(r3 != y.append(", World").toString) /** should be equal */
    assert(r4 != y.append(", World").toString) /** should be equal */


    /**  pure function */
    def abs(n: Int) = if (n <= 0) -n else n

    /** pure function */
    def factorial(n: Int):Int ={
      @tailrec
      def loop(n:Int,acc:Int):Int={
        if(n<=0) acc
        else loop(n-1,n*acc)
      }
      loop(n,1)
    }

    def fibonacci(n: Int):Int ={
      def loop(n:Int):Int={
        if(n <= 1) n
        else loop(n-1) + loop(n-2)
      }
      loop(n)
    }


    /** Functions are objects too. They can be passed around like any other value,
      assigned to variables, stored in data structures, and so on.
     */

    def formatAbs(x: Int) = {
      val msg = s"The absolute value of %d is %d"
      msg.format(x, abs(x))
    }
    def formatFactorial(x: Int) = {
      val msg = s"The factorial of %d is %d"
      msg.format(x, factorial(x))
    }

    def formatResult(x: Int, f:Int=>Int, name:String) = {
      val msg = s"$name of %d is %d"
      msg.format(x, f(x))
    }

    assert(formatAbs(-42) == formatResult(-42, abs, "The absolute value"))
    assert(formatFactorial(5) == formatResult(5, factorial, "The factorial"))
    assert(fibonacci(7) == 13)
    assert(fibonacci(25) == 75025)

  }
}
