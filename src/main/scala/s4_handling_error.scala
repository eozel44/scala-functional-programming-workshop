object s4_handling_error{

/**
    // ': =>' the argument will not be evaluated until it is needed
   //  'B>:A' parameter on these functions indicates that B must be a supertype of A.
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



  def main(args: Array[String]): Unit = {

    def mean(l:Seq[Double]):Option[Double] ={
      if (l.isEmpty) None
      else Some(l.sum / l.size)
    }

    val li = Seq(10d, 15d, 20d)
    assert(mean(li) == Some(15d))

    val k = None
    assert(k.getOrElse(1) == 1)

  }
}
