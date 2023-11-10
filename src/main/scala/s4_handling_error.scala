object s4_handling_error{

/**
   Option
 */
sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match{
    case None => None
    case Some(a) => Some(f(a))
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }
  // 'default: => B' equals 'Unit => B'
  //  'B>:A' parameter on these functions indicates that B must be a supertype of A.
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => Some(a)
    case _ => None
  }

  def orElseV2[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

}
case object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A]

/**
 * Either
 * */
 sealed trait Either[+E, +A]{
   def map[B](f:A=>B):Either[E,B] = this match {
     case Left(e) => Left(e)
     case Right(a) => Right(f(a))
   }
   def flatMap[EE >: E, B](f:A=>Either[EE, B]):Either[EE, B] = this match{
     case Left(e) => Left(e)
     case Right(a) => f(a)
   }
   def orElse[EE>:E, B>:A](b: => Either[EE, B]):Either[EE,B]= this match{
     case Left(_) => b
     case Right(a) => Right(a)
   }

   def map2[EE>:E,B,C](b: => Either[EE,B])(f:(A,B)=>C):Either[EE,C] =for{
     a <-this
     b1 <- b
   } yield f(a,b1)

 }
 case class Left[+E](value: E) extends Either[E, Nothing]
 case class Right[+A](value: A) extends Either[Nothing, A]

  def main(args: Array[String]): Unit = {


    /** option sample **/
    def mean(l:Seq[Double]):Option[Double] ={
      if (l.isEmpty) None
      else Some(l.sum / l.size)
    }

    val li = Seq(10d, 15d, 20d)
    assert(mean(li) == Some(15d))

    val k = None
    assert(k.getOrElse(1) == 1)

    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil => Some(Nil)
      case head :: tail => head.flatMap(hh=> sequence(tail).map(hh :: _))
    }

    val lis = List(Some(1),Some(2),Some(3))
    assert(sequence(lis) == Some(List(1,2,3)))

    /** either sample */
    case class Person(name: Name, age: Age)
    case class Name(val value: String)
    case class Age(val value: Int)

    def mkName(name: String): Either[String, Name] =
      if (name == "" || name == null) Left("Name is empty.")
      else Right(Name(name))
    def mkAge(age: Int): Either[String, Age] =
      if (age < 0) Left("Age is out of range.")
      else Right(Age(age))

    def mkPerson(name: String, age: Int): Either[String, Person] =
      mkName(name).map2(mkAge(age))(Person(_, _))

    assert(mkPerson("eren",18) == Right(Person(Name("eren"),Age(18))))
    assert(mkPerson("eren",-1) == Left("Age is out of range."))
    assert(mkPerson("",20) == Left("Name is empty."))

  }
}
