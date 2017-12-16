package fpinscala.testing

object Coursera extends App {

  val xs = List(1,1,2,3,4,4,4,5,5,5,5,1,1,1,1)
  println("dropWhile", xs dropWhile { _ < 4 })
  println("takeWhile", xs takeWhile { _ < 4 })
  println("span", xs span { _ < 4 })

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x::_ => 
      val (first, rest) = xs span ( _ == x) 
      first :: pack(rest)
  }

  def encode[T](xs: List[T]): List[(T, Int)] = 
    pack(xs) map (xs1 => (xs1.head, xs1.size))

  println("pack", pack(xs))  
  println("encode", encode(List("a", "a", "a", "b", "c", "c", "a", "a", "d", "d")))

}

