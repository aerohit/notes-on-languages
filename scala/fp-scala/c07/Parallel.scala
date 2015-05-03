object ParTrialAndError {
  def sumSerial(ints: Seq[Int]): Int = ints.foldLeft(0)(_ + _)
  def sumDivideConquer(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      sumDivideConquer(l) + sumDivideConquer(r)
    }

  def unit[A](a: => A): Par[A]
  def run[A](a: Par[A]): A

  def sumPar1(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      val sumL: Par[Int] = unit(sumPar(l))
      val sumR: Par[Int] = unit(sumPar(r))
      run(sumL) + run(sumR)
    }


  // -----> Exercise 7.1
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]

  def sumPar2(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(sumPar2(l), sumPar2(r))(_ + _)
    }

  // notice this version is non-strict
  def unit1[A](a: A): Par[A]
  def fork[A](a: => Par[A]): Par[A]
  def lazyUnit[A](a: => A): Par[A] = fork(unit1(a))

  def sumPar3(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      unit1(ints.headOption getOrElse 0) // notice the non-strict use of unit
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(fork(sumPar3(l)), fork(sumPar3(r)))(_ + _)
    }
}

// -----> Exercise 7.2
type Par[A] = ExecutorService => Future[A]

object Par {
  def unit[A](a: A): Par[A] = (ex: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }

  def fork[A](a: => Par[A]): Par[A] = (es: ExecutorService) =>
    es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  // TODO -----> Exercise 7.3

  // -----> Exercise 7.4
  def asyncF[A,B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar[A](parList: Par[a]): Par[A] = map(parList)(_.sorted)

  // -----> Exercise 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldLeft[Par[List[A]]](unit(Nil))((h,t) => map2(h,t)(_::_))

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  // TODO -----> Exercise 7.6
  def parFilter[A](ps: List[A])(f: A => Boolean): Par[List[A]]
}
