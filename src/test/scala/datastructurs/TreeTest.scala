package datastructurs

import org.scalatest._

class TreeTest extends FunSuite with Matchers {
  test("testDepth") {
    val tree = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    Tree.depth(tree) should equal(2)
    Tree.depth(Branch(tree, Branch(Leaf(8), Leaf(6)))) should equal(3)
  }

  test("testMaximum") {
    val tree = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    Tree.maximum(tree) should equal(3)
    Tree.maximum(Branch(tree, Branch(Leaf(8), Leaf(6)))) should equal(8)
  }

  test("testSize") {
    val tree = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    Tree.size(tree) should equal(5)
    Tree.size(Branch(tree, Leaf(12))) should equal(7)
  }
}
