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

// Mutable in-place selection sort
def mutableSelectionSort(elements: Vector[Int]): Vector[Int] = {
  def minIndex(elements: Seq[Int], start: Int, end: Int): Int = {
    var i = start
    var min = start
    while (i < end) {
      if (elements(i) < elements(min)) min = i
      i += 1
    }
    min
  }

  val buffer = elements.toBuffer
  val maxLength = buffer.length
  for (i <- 0 until maxLength) {
    val idx = minIndex(buffer, i, maxLength)
    // switch elements in place
    val temp = buffer(i)
    buffer(i) = buffer(idx)
    buffer(idx) = temp
  }
  buffer.toVector
}

mutableSelectionSort(Vector(1, 2, 3, 4, 4))
