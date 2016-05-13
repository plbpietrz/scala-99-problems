object Problems11to20 {
  def encodeModified[T](xs: List[T]): List[Any] = {
    Problems01to10.encode(xs).map {
      case (n, x) => if (n == 1) x else (n, x)
    }
  }

  def decode[T](xs: List[(Int, T)]): List[T] = {
    def aux[T](n: Int, a: T, as: List[T]): List[T] = {
      if (n == 0) as else aux(n-1, a, a::as)
    }
    xs.foldLeft(List[T]())((a, x) => aux(x._1, x._2, a)).reverse
  }

  def decode_sol[T](xs: List[(Int, T)]): List[T] = {
    xs flatMap { e => List.make(e._1, e._2) }
  }

  def encodeDirect[T](xs: List[T]): List[(Int,T)] = xs match {
    case Nil         => List()
    case _ : List[T] => {
      val (p, nx) = xs span (_ == xs.head)
      (p length, p head)::encodeDirect(nx)
    }
  }

  def duplicate[T](xs: List[T]): List[T] = xs.flatMap(x => List(x,x))

  def duplicateN[T](n: Int, xs: List[T]): List[T] = xs.flatMap(List.make(n,_))

  def drop[T](n: Int, xs: List[T]): List[T] = {
    def aux[T](i: Int, xs: List[T], ys: List[T]): List[T] = xs match {
      case x::xs => if (i == n) aux(1, xs, ys) else aux(i + 1, xs, x::ys)
      case _     => ys
    }
    aux(1, xs, List[T]()).reverse
  }

  def dropFunctional[A](n: Int, ls: List[A]): List[A] =
    ls.zipWithIndex filter { v => (v._2 + 1) % n != 0 } map { _._1 }

  def split[T](n: Int, xs: List[T]): (List[T], List[T]) =
    (xs take n, xs drop n)

  def slice[T](s: Int, e: Int, xs: List[T]): List[T] = {
    def aux[T](i: Int, xs: List[T], ys: List[T]): List[T] = xs match {
      case x::xs => if (i < s) aux(i + 1, xs, ys)
                    else if (i < e) aux(i + 1, xs, x::ys)
                    else ys
      case _     => ys
    }
    aux(0, xs, Nil).reverse
  }

  def rotate[T](n: Int, xs: List[T]) = {
    if (n > 0) xs.splitAt(n)
    else       xs.splitAt(xs.length + n)
    match { case (xs,ys) => ys:::xs }
  }

  def removeAt[T](n: Int, xs: List[T]) = {
    xs.splitAt(n) match {case (xs, ys) => (xs:::(ys tail), ys head)}
  }

}
