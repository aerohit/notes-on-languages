// Monoid consists of the following laws
// + Some type A
// + An assosiative binary operation, "op", such that:
//    op(op(x,y), z) == op(x, op(y, z))
//   for any choice of x, y, z of type A
// + A value "zero" such that:
//    op(x, zero) == op(zero, x) == x
//   for any choice of x: A

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    def zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    def zero = Nil
  }

  // -----> Exercise 10.1

  val intAdditions = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    def zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    def zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    def zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    def zero = true
  }

  // -----> Exercise 10.2

  // following is a first-option biased monoid
  // we can define a last-option biased monoid as well
  // and we can also define duals of monoid
  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
    def zero = None
  }

  // -----> Exercise 10.3

  // again it's a biased definition, it could be defined as
  // a2 compose a1
  def endoMonoid[A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A) = a1 compose a2
    def zero = identity
  }

  // -- TODO -----> Exercise 10.4
  //

  val words = List("Do", "Re", "Me")
  val s = words.foldLeft(stringMonoid.zero)(stringMonoid.op)
  val t = words.foldRight(stringMonoid.zero)(stringMonoid.op)

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  // -----> Exercise 10.5

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = concatenate(as.map(f), m)

  // -- TODO -----> Exercise 10.6
  //

  // TODO Read till section 10.2
}
