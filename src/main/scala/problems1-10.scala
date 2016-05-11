object Problems1to10 {
  @annotation.tailrec
  def last[T](xs: List[T]): T = xs match {
    case x::Nil => x
    case x::xs  => last(xs)
    case _      => throw new NoSuchElementException
  }

  @annotation.tailrec
  def penultimate[T](xs: List[T]): T = xs match {
    case x::_::Nil => x
    case x::xs     => penultimate(x::xs)
    case _         => throw new NoSuchElementException
  }

  @annotation.tailrec
  def nth[T](n: Int, xs: List[T]): T = xs match {
    case x::xs     => if (n == 0) x else nth(n - 1, xs)
    case _         => throw new NoSuchElementException
  }

  def length[T](xs: List[T]): Int = {
    @annotation.tailrec
    def aux[T](l: Int, xs: List[T]): Int = xs match {
      case Nil   => l
      case _::xs => aux(l + 1, xs)
    }
    aux(0, xs)
  }

  def reversereverse[T](xs: List[T]): List[T] = {
    @annotation.tailrec
    def aux[T](xs: List[T], ys: List[T]): List[T] = xs match {
      case Nil   => ys
      case x::xs => aux(xs, x::ys)
    }
    aux(xs, Nil)
  }

  def reversereverseFunc[T](xs: List[T]): List[T] = {
    xs.foldLeft(List[T]())((a, x) => x::a)
  }

  def isPalindrome[T](xs: List[T]): Boolean = {
    xs equals reversereverseFunc(xs)
  }

  def flatten(xs: List[Any]): List[Any] = xs flatMap {
    case xs: List[_] => flatten(xs)
    case x           => List(x)
  }

  def compress[T](xs: List[T]): List[T] = {
    xs.foldLeft(List[T]())((as, e) => as match {
      case Nil => List(e)
      case x::xs => if (x equals e) as else e::as
    }).reverse
  }

  def pack[T](xs: List[T]): List[List[T]] = {
    @annotation.tailrec
    def aux(xs: List[T], e: T, es: List[T], as: List[List[T]]): List[List[T]] = xs match {
      case x::xs => if (x equals e) aux(xs, e, x::es, as) else aux(xs, x, List(x), es::as)
      case Nil   => es::as
    }
    aux(xs tail, xs head, List(xs head), List()).reverse
  }

  def encode[T](xs: List[T]): List[(Int, T)] = {
    pack(xs).foldLeft(List[(Int,T)]())((a, x) => (length(x), x head)::a).reverse
  }

}
