package datastructurs

import org.scalatest._

class TreeTest extends FunSuite with Matchers {
  test("structure") {
    val tree = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    Tree.size(tree) should equal(5)
    Tree.size(Branch(tree, Leaf(12))) should equal(7)
  }
}
