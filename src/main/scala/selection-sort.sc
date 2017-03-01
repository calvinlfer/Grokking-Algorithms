import scala.annotation.tailrec

def selectionSort(elements: Vector[Int]): Vector[Int] = {
  def min(elements: Vector[Int]): (Int, Vector[Int]) = {
    val min = elements.min
    (min, elements.filter(e => e != min))
  }

  if (elements.isEmpty) elements
  else {
    val (smallest, rest) = min(elements)
    smallest +: selectionSort(rest)
  }
}

// Tail recursive
def selectionSortV2(elements: Vector[Int]): Vector[Int] = {
  def min(elements: Vector[Int]): (Int, Vector[Int]) = {
    val min = elements.min
    (min, elements.filter(e => e != min))
  }

  @tailrec
  def inner(working: Vector[Int], acc: Vector[Int]): Vector[Int] = {
    if (working.isEmpty) acc
    else {
      val (smallest, rest) = min(working)
      inner(rest, acc :+ smallest)
    }
  }
  inner(elements, Vector.empty)
}

selectionSortV2(Vector(5, 3, 2, 1))
