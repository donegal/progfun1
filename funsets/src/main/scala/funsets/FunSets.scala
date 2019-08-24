package funsets

/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = e => e == elem

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = e => s(e) || t(e)

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = e => s(e) && t(e)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = e => s(e) && !t(e)

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = e => s(e) && p(e)

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (s(a) && !p(a)) false
      else iter(a + 1)
    }
    iter(-bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`. we want to fail a test that all bounded
   * integers are not 'p'. that would guarantee that we had one.
   */
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, !p(_))

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   * this was the most functional problem. it requires us to write a
   * new anonymous function for the second parameter to exists().
   *  s 							: original set
   * 	e 							: test element to include in result Set
   *  x 							: original set element inside exists
   *  f(x) 						: applied function inside exists
   *  x => f(x) == e 	: include e if there exists some original
   *  								  set element that equals e after applying
   *  									the function. 
   */
  def map(s: Set, f: Int => Int): Set = e => exists(s, x => f(x) == e)

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
