object Problems11to20 {
  def encodeModified[T](xs: List[T]): List[Any] = {
    Problems1to10.encode(xs).map {
      case (n, x) => if (n == 1) x else (n, x)
    }
  }

  def decode[T](xs: List[(Int, T)]): List[T] = {
    Nil
  }

}
