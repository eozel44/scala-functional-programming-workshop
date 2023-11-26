package state


object s6_state{

  def main(args: Array[String]): Unit = {

    /**
       how to write programs that manipulate state in a purely functional way,
       using the very simple domain of random number as the example

       we can assume that the object rng has some internal state that gets updated after each invocation,
       since we would otherwise get the same value each time we call nextInt or nextDouble.
       Because the state updates are performed as a side effect, these methods are not referentially transparent.

       rng.nextDouble rng.nextInt

     */

    val rngSimple = new scala.util.Random

    /**
         Purely functional random number generation

         Rather than returning only the generated random number (as is done in
         scala.util.Random) and updating some internal state by mutating it in place,
         we return the random number and the new state, leaving the old state unmodified.
         In effect, we separate the computing of the next state from the concern of passing
         the new state through the program.

     */

    trait RNG {
      def nextInt: (Int, RNG)
    }

     // linear congruential generator algorithm
    object RNG {
      def simple(seed: Long): RNG = new RNG {
        def nextInt: (Int, RNG) = {
          val seed2 = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)

          ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
        }

        def positiveInt(rng: RNG): (Int, RNG) ={
          val (i, r) = rng.nextInt
          (if (i < 0) - (i + 1) else i, r)
        }

        def double(rng: RNG): (Double, RNG) ={
          val (i, r) = positiveInt(rng)
          (i / (Int.MaxValue.toDouble + 1), r)
        }
        def boolean(rng: RNG): (Boolean, RNG) =
          rng.nextInt match { case (i,rng2) => (i%2 == 0,rng2) }
      }
      def inits(count:Int)(rng:RNG):(List[Int],RNG) ={

        @scala.annotation.tailrec
        def go(count:Int, r:RNG, xs:List[Int]):(List[Int],RNG)={
          if(count==0)
            (xs,r)
          else{
            val (x,r2) = r.nextInt
            go(count-1,r2,x::xs)
          }
        }
        go(count,rng,List())
      }
    }

    /**
        We can run this sequence of statements as many times as we want and we will always get the same values.
        When we call rng.nextInt it will always return 16159453 and a new RNG whose nextInt will always return -1281479697.
    */
    val rng = RNG.simple(42)
    val (n1, rng2) = rng.nextInt
    //n1: Int = 16159453
    //rng2: RNG = RNG$$anon$1@6ebc4e13
    val (n2, rng3) = rng2.nextInt
    //n2: Int = -1281479697
    //rng3: RNG = RNG$$anon$1@24e3e386


    /**
       Making stateful APIs pure

       This problem of making seemingly stateful APIs pure, and its solution, of having
       the API compute the next state rather than actually mutate anything,
       is not unique to random number generation.
       It comes up quite frequently, and we can always deal with it in this same way.
     */


    trait Bar

//    class Foo{
//      var s: FooState = ???
//      def bar: Bar
//      def baz: Int
//    }

/**  We can mechanically translate this to the purely functional API:  */
    trait Foo {
      def bar: (Bar, Foo)
      def baz: (Int, Foo)
    }

  }
}