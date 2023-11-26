package state


object s6_state {

  def main(args: Array[String]): Unit = {

    /**
     * how to write programs that manipulate state in a purely functional way,
     * using the very simple domain of random number as the example
     *
     * we can assume that the object rng has some internal state that gets updated after each invocation,
     * since we would otherwise get the same value each time we call nextInt or nextDouble.
     * Because the state updates are performed as a side effect, these methods are not referentially transparent.
     *
     * rng.nextDouble rng.nextInt
     *
     */

    val rngSimple = new scala.util.Random

    /**
     * Purely functional random number generation
     *
     * Rather than returning only the generated random number (as is done in
     * scala.util.Random) and updating some internal state by mutating it in place,
     * we return the random number and the new state, leaving the old state unmodified.
     * In effect, we separate the computing of the next state from the concern of passing
     * the new state through the program.
     *
     */

    trait RNG {
      def nextInt: (Int, RNG)
    }

    // linear congruential generator algorithm
    object RNG {
      case class Simple(seed: Long) extends RNG {
        def nextInt: (Int, RNG) = {
          val seed2 = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)

          ((seed2 >>> 16).asInstanceOf[Int], Simple(seed2))
        }
      }

      def   positiveInt(rng: RNG): (Int, RNG) = {
        val (i, r) = rng.nextInt
        (if (i < 0) -(i + 1) else i, r)
      }

      def double(rng: RNG): (Double, RNG) = {
        val (i, r) = positiveInt(rng)
        (i / (Int.MaxValue.toDouble + 1), r)
      }

      def boolean(rng: RNG): (Boolean, RNG) =
        rng.nextInt match {
          case (i, rng2) => (i % 2 == 0, rng2)
        }

      def inits(count: Int)(rng: RNG): (List[Int], RNG) = {

        @scala.annotation.tailrec
        def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) = {
          if (count == 0)
            (xs, r)
          else {
            val (x, r2) = r.nextInt
            go(count - 1, r2, x :: xs)
          }
        }

        go(count, rng, List())
      }

      def intDouble(rng: RNG): ((Int, Double), RNG) = {
        val (i, r1) = rng.nextInt
        val (d, r2) = double(r1)
        ((i, d), r2)
      }

      def doubleInt(rng: RNG): ((Double, Int), RNG) = {
        val ((i, d), r) = intDouble(rng)
        ((d, i), r)
      }

      /** we notice a common pattern: each of our functions
          has a type of the form for RNG => (A, RNG) some type A. */

      type Rand[+A] = RNG => (A, RNG)

      val int: Rand[Int] = _.nextInt

      def unit[A](a: A): Rand[A] = rng => (a, rng)

      /** These state actions can be combined using combinators
          which are higher-order functions that we will define in this section. */
      def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }

      val _double: Rand[Double] = map(positiveInt)(_ / (Int.MaxValue.toDouble + 1))

      def positiveEven:Rand[Int] =map(positiveInt)(i=>i-i % 2)

      /**
         map is not powerful enough to implement intDouble and doubleInt functions
         What we need is a new combinator map2, that can
         combine two RNG actions into one using a binary rather than unary function.
       */

      def map2[A,B,C](ra:Rand[A], rb:Rand[B])(f:(A,B)=>C):Rand[C] = rng=> {
        val (a, r1) = ra(rng)
        val (b, r2) = rb(r1)
        (f(a, b), r2)
      }

      def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
          map2(ra, rb)((_, _))

      val randIntDouble: Rand[(Int, Double)] = both(int,double)

      val randDoubleInt: Rand[(Double, Int)] = both(double,int)

      def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng =>{
        val (a, r1) = f(rng)
        g(a)(r1) // We pass the new state along
      }

      def nonNegativeLessThan(n: Int): Rand[Int] = {
        flatMap(positiveInt) { i =>
          val mod = i % n
          if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
        }
      }

      def _map[A,B](s: Rand[A])(f: A => B): Rand[B] =
        flatMap(s)(a => unit(f(a)))

      def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
        flatMap(ra)(a => map(rb)(b => f(a, b)))


    }

    /**
        We can run this sequence of statements as many times as we want and we will always get the same values.
        When we call rng.nextInt it will always return 16159453 and a new RNG whose nextInt will always return -1281479697.
    */
    val rng = RNG.Simple(42)
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