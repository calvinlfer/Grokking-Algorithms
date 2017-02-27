import scala.annotation.tailrec

def binarySearch(lookingFor: Int, input: Vector[Int]): Option[Int] = {
  @tailrec
  def go(start: Int, end: Int): Option[Int] = {
    val mid: Int = (end + start) / 2
    if (start == end && input(start) != lookingFor) None
    else if (lookingFor == input(mid)) Some(mid)
    else if (lookingFor < input(mid)) go(start, start + mid-1)
    else go(mid+1, end)
  }
  go(0, input.length-1)
}

val list = Vector(-1, 2, 3, 4, 5, 6, 7, 8, 9)

binarySearch(7, list)
  
