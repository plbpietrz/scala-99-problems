object Problems11to20 {
  def encodeModified[T](xs: List[T]): List[Any] = {
    Problems1to10.encode(xs).map {
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

}
