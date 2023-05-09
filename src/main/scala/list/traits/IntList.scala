package list.traits

/**
  * Specifies an immutable list of integers.
  */
trait IntList {

  /**
    * Indicates if the list contains no elements.
    *
    * @return true if the list contains no elements
    */
  def isEmpty: Boolean

  /**
    * Returns the first element of the list.
    *
    * @return the first element
    */
  def head: Int

  /**
    * Returns all elements except the first one.
    *
    * @return all elements except the first one
    */
  def tail: IntList

  /**
    * Returns the number of elements in the list.
    *
    * @return the number of elements in the list
    **/
  def size: Int

  /**
    * Returns the n-th element in the list. The position of the head element is considered 0.
    *
    * @param index the zero based index
    * @return the n-th element
    */
  def get(index: Int): Int

  /**
    * Indicates if the given element is in the list.
    *
    * @param elem the element to check
    * @return true if the element is in the list, false otherwise
    */
  def contains(elem: Int): Boolean

  /**
    * Returns a new list with the given element added at the end of the list.
    *
    * @param elem the element to add at the end
    * @return a new list with the old list, followed by the element
    */
  def append(elem: Int): IntList

  /**
    * Returns a new list with the given element added at the start of the list.
    *
    * @param elem the element to prepend
    * @return a new list with the element at the start followed by the old list
    */
  def prepend(elem: Int): IntList

  /**
    * Returns a new list without the first occurrence of the element.
    *
    * @param elem the element
    * @return a new list without the first occurrence of the element
    */
  def delete(elem: Int): IntList

  /**
    * Returns a new list without all occurrences of the element.
    *
    * @param elem the element
    * @return a new list without all occurrences of the element
    */
  def deleteAll(elem: Int): IntList

  /**
    * Return a new list which contains all elements of other followed by all elements of this.
    *
    * @param other the other list
    * @return a new list, with the elements of both lists
    */
  def prefix(other: IntList): IntList

  /**
    * Applies a unary function to all elements of the list
    * and returns a new list containing the new values.
    *
    * @param mapFunc the function to apply to all elements
    * @return a new list containing the new values
    */
  def map(mapFunc: Int => Int): IntList

  /**
    * Returns a new list which only contains the elements for which predicateFun returns true.
    *
    * @param predicateFunc the predicate function
    * @return a new list containing only the elements, which fulfil the predicate
    */
  def filter(predicateFunc: Int => Boolean): IntList

  /**
    * Applies a binary operation to the start value and all elements in the list going left to right.
    *
    * @param reduceFunc the binary operation
    * @return the results of inserting reduceFunc between consecutive elements
    *         of the list starting with initial on the left
    * @example op(op(op(op(initial, x1), x2), x3), x4) where the xs are the elements of the list
    */
  def foldLeft(initial: Int)(reduceFunc: (Int, Int) => Int): Int

  /**
    * Applies a binary operation to all elements in the list going left to right.
    *
    * @param reduceFunc the binary operation
    * @return the results of inserting reduceFunc between consecutive elements of the list
    * @example op(op(op(x1,x2),x3),x4) where the xs are the elements of the list
    */
  def reduceLeft(reduceFunc: (Int, Int) => Int): Int

  /**
    * Returns a list that contains all elements in the opposite direction.
    *
    * @return a new flipped list
    */
  def flip: IntList

  /**
    * Returns a new list which is equal to the sorted list using insertion sort.
    *
    * @return a new sorted list
    */
  def insertionSort: IntList

  /**
    * Returns a new list with the given element inserted at the right position in a sorted list.
    *
    * @param elem the element to insert
    * @return a new list with the element in the right position
    */
  def insertSorted(elem: Int): IntList

  /**
    * Returns true if all elements in the list fulfil the predicate.
    * If the predicate returns false for a element, the check for further elements is omitted and false is returned.
    *
    * @return true if all elements fulfil the predicate
    */
  def forAll(predicateFunc: Int => Boolean): Boolean

  /**
    * Applies a binary operation to the start value and all elements in the list going right to left.
    *
    * @param reduceFunc the binary operation
    * @return the results of inserting reduceFunc between consecutive elements
    *         of the list starting with initial on the left
    * @example op(x1,op(x2,op(x3,op(x4,initial)))) where the xs are the elements of the list
    */
  def foldRight(initial: Int)(reduceFunc: (Int, Int) => Int): Int

  /**
    * Applies a binary operation to all elements in the list going right to left.
    *
    * @param reduceFunc the binary operation
    * @return the results of inserting reduceFunc between consecutive elements of the list
    * @example op(x1,op(x2,op(x3,x4))) where the xs are the elements of the list
    */
  def reduceRight(reduceFunc: (Int, Int) => Int): Int

}