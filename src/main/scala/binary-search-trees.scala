import scala.annotation.tailrec

case class Node(left: Option[Node], value: Int, right: Option[Node])

def rootNode(value: Int) = Some(Node(None, value, None))

@tailrec
def search(element: Int, data: Node): Option[Node] =
  data match {
    case n @ Node(_, value, _) if value == element => Some(n)
    case Node(Some(left), value, _) if element < value => search(element, left)
    case Node(_, value, Some(right)) if element > value => search(element, right)
    case _ => None
  }

val `10/14\19` = Node(rootNode(10), 14, rootNode(19))
val `31/35\42` = Node(rootNode(31), 35, rootNode(42))
val `14t/27\35t` = Node(Some(`10/14\19`), 27, Some(`31/35\42`))

search(27, `14t/27\35t`)
// Some(root of the tree)

search(10, `14t/27\35t`)
// Some(Node(None,10,None))

search(31, `14t/27\35t`)
// Some(Node(None,31,None))


// functional insertion with data sharing
def insert(element: Int, tree: Node): Node =
  tree match {
    // leaves
    case n @ Node(_, value, None) if element > value =>
      n.copy(right = rootNode(element))

    case n @ Node(None, value, _) if element < value =>
      n.copy(left = rootNode(element))

    // thou shalt not mutate, but create
    // thou shalt re-create what you touch
    case n @ Node(_, value, Some(rightTree)) if element > value =>
      n.copy(right = Some(insert(element, rightTree)))

    case n @ Node(Some(leftTree), value, _) if element < value =>
      n.copy(left = Some(insert(element, leftTree)))
  }

val treeWith32 = insert(32, `14t/27\35t`)
