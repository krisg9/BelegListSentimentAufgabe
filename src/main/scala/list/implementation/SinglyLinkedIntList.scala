package list.implementation

import list.traits.IntList

/**
 * A companion object for the singly linked list.
 * This enables creating lists list this: val list = SinglyLinkedIntList(1,2,3)
 * which results in Cons(1,Cons(2,Cons(3,Empty))))
 */
object SinglyLinkedIntList {


  /** The apply function is a special function in scala.
   * It can be invoked with SinglyLinkedIntList.apply(args) or simply SinglyLinkedIntList(args).
   * This particular implementation of it is also a variadic function, i.e.
   * a function which accepts one or more arguments of the same type (integers) as parameters.
   */
  //inside this method xs is of type Seq[int]
  def apply(xs: Int*): SinglyLinkedIntList = xs match {
    case Seq() => Empty
    //: _* results in the sequence being passed as multiple parameters - (1,2,3) instead of Seq[Int]{1,2,3}
    case _ => Cons(xs.head, SinglyLinkedIntList(xs.tail: _*))
  }
}

abstract class SinglyLinkedIntList extends IntList {

  override def prefix(other: IntList): IntList = other match {

    case Empty => this
    case Cons(h, t) => Cons(h, prefix(t))
  }

  override def size: Int = this match {

    case Empty => 0
    case _ => 1 + tail.size
  }

  override def map(mapFunc: Int => Int): IntList = this match {

    case Empty => Empty
    case Cons(_, _) => Cons(mapFunc(head), tail.map(mapFunc))
  }

  override def filter(filterFunc: Int => Boolean): IntList = this match {

    case Empty => Empty
    case Cons(_, _) => if (filterFunc(head)) Cons(head, tail.filter(filterFunc)) else tail.filter(filterFunc)
  }

  override def foldLeft(initial: Int)(reduceFunc: (Int, Int) => Int): Int = this match {

    case Empty => initial
    case Cons(_, _) => tail.foldLeft(reduceFunc(initial, head))(reduceFunc)
  }

  /** *
   *
   * Beleg 1
   */

  override def foldRight(initial: Int)(reduceFunc: (Int, Int) => Int): Int = ???

  override def reduceLeft(reduceFunc: (Int, Int) => Int): Int = ???

  override def reduceRight(reduceFunc: (Int, Int) => Int): Int = ???

  override def forAll(predicateFunc: Int => Boolean): Boolean = ???

  override def insertSorted(elem: Int): IntList = ???

  override def insertionSort: IntList = ???
}