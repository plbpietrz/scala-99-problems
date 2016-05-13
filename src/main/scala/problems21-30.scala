object Problems21to30 {

  def insertAt[T](i: T, n: Int, xs: List[T]): List[T] = {
    xs.splitAt(n) match {
      case (xs, ys) => xs:::i::ys
    }
  }

  def range(s: Int, e: Int): List[Int] = {
    def aux(x: Int, xs: List[Int]): List[Int] = {
      if (s == x) x::xs else aux(x - 1, x::xs)
    }
    aux(e, Nil)
  }

  def randomSelect[T](n: Int, xs: List[T]): List[T] = {
    def aux[T](n: Int, xs: List[T], ys: List[T], r: scala.util.Random): List[T] = n match {
      case 0      => ys
      case _: Int => {
        val (zs, i) = Problems11to20.removeAt(r.nextInt(xs length), xs)
        aux(n - 1, zs, i::ys, r)
      }
    }
    aux(n, xs, Nil, new scala.util.Random)
  }

  def lotto(n: Int, l: Int): List[Int] = {
    randomSelect(n, range(1, l + 1))
  }

  def randomPermute[T](xs: List[T]): List[T] =
    randomSelect(xs length, xs)
}
