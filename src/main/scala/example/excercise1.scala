//excercise 1: Implementing a tree data structure with algebraic data types
//“A Tree with elements of type A is:
//
//a Leaf with a value of type A; or
//a Node with a left and right child, which are both Trees with elements of type A.”

//Excerpt From
//Functional Programming Strategies
//Noel Welsh
//This material may be protected by copyright.

object Tree {
  sealed trait Tree[A] extends Product with Serializable
  final case class Leaf[A](value: A) extends Tree[A]
  final case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Node(left, right) => size(left) + size(right)
  }

  def contains[A](tree: Tree[A], value: A): Boolean = tree match {
    case Leaf(v) => v == value
    case Node(left, right) => contains(left, value) || contains(right, value)
  }

  def map [A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match{
    case Leaf(v) => Leaf(f(v))
    case Node(left, right) => Node(map(left)(f), map(right)(f))
  }

  def foldRight[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(v) => f(v)
    case Node(left, right) => g(foldRight(left)(f)(g), foldRight(right)(f)(g))
  }
}
