/**
 * Created by Joe on 3/22/14.
 */

object Problems {

  val list: List[Int] = List(1, 2, 3, 4, 5, 45, 17)
  val charsList: List[Char] = List('a', 'b', 'c', 'd', 'e', 'f')
  val simpleList: List[Char] = List('a', 'b', 'c', 'c', 'd')
  val encodingList: List[Char] = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')

  // Problem 1 -- Find last in list
  def last[T](values: List[T]): T = values.last

  last(list)
  last(charsList)

  // Problem 2 -- Find penultimate in List
  def penultimate[T](values: List[T]): T = values.takeRight(2).head

  penultimate(list)
  penultimate(charsList)

  //Problem 3 -- Find nth in list
  def nth[T](n: Int, values: List[T]): T = values(n)

  nth(3, list)
  nth(3, charsList)

  // Problem 4 -- Find number of elements in list
  def len[T](values: List[T]): Int = values.length

  len(list)
  len(charsList)

  // Problem 5 -- Reverse input list
  def reverse[T](values: List[T]): List[T] = values.reverse

  reverse(list)
  reverse(charsList)

  //Problem 6 -- Determine if list is a palindrome
  def isPalindrome[T](values: List[T]): Boolean = values.equals(values.reverse)

  isPalindrome(list)
  isPalindrome(charsList)
  isPalindrome("racecar".toList)
  isPalindrome(Nil)

  // Problem 7 -- Flatten a nested structure
  def makeFlat(values: List[Any]): List[Any] = values flatMap {
    case stuff: List[_] => makeFlat(stuff)
    case thing => List(thing)
  }

  makeFlat(List(1, List(2)))

  // Problem 8 -- Eliminate consecutive duplicates
  def removeConsecDup[T](values: List[T]): List[T] = values match {
    case Nil => Nil
    case h :: hs => h :: removeConsecDup(hs.dropWhile(_ == h))
  }

  def removeConsecDup2[T](values: List[T]): List[T] = values.foldRight(List[T]()) {
    (x, y) => if (y.isEmpty || y.head != x) x :: y else y
  }

  removeConsecDup(encodingList)
  removeConsecDup2(encodingList)

  // Problem 9 -- Pack consecutive duplicates of list elements into sublists.
  def packConsecDup[T](values: List[T]): List[Any] = values match {
    case Nil => Nil
    case x :: xs => (x :: xs).takeWhile(_ == x) :: packConsecDup(xs.dropWhile(_ == x))
  }

  packConsecDup(encodingList)

  // Problem 10 -- Pack consecutive duplicates of list elements into sublists.
  def encode[T](values: List[T]): List[(Int, T)] = values match {
    case Nil => Nil
    case x :: xs => ((x :: xs).takeWhile(_ == x).length, x) :: encode(xs.dropWhile(_ == x))
  }

  encode(encodingList)

  // Problem 11 -- Problem 10 but don't have list with 1
  def encode2[T](ls: List[T]): List[Any] = {
    encode(ls) map {
      t => if (t._1 == 1) t._2 else t
    }
  }

  encode2(encodingList)

  // Problem 12 -- Decode a run-length encoded list.
  def decode[T](ls: List[(Int, T)]): List[T] = {
    ls.flatMap((x) => List.fill(x._1)(x._2))
  }

  encodingList == decode(encode(encodingList))

  // Problem 13 -- Run-length encoding of a list (direct solution).
  def encodeDirect[T](ls: List[T]): List[(Int, T)] =
    if (ls.isEmpty) Nil
    else {
      val (packed, next) = ls span {
        _ == ls.head
      }
      (packed.length, packed.head) :: encodeDirect(next)
    }

  encodeDirect(encodingList)

  // Problem 14 -- Duplicate the elements of a list.
  def duplicateElems[T](ls: List[T]): List[T] = ls match {
    case Nil => Nil
    case x :: xs => x :: x :: duplicateElems(xs)
  }

  // Problem 14's proposed answer
  def duplicateElems2[T](ls: List[T]): List[T] = ls flatMap (x => List(x, x))

  duplicateElems(simpleList)
  duplicateElems2(simpleList)

  // Problem 15 -- Duplicate the elements of a list a given number of times.
  def duplicateElemNTimes[T](n: Int, ls: List[T]): List[T] =
    ls flatMap (x => List.fill(n)(x))

  duplicateElemNTimes(3, simpleList)

  //Problem 16 -- Drop every Nth element from a list.
  def drop[T](n: Int, ls: List[T]): List[T] = {
    ls.zipWithIndex filter (v => (v._2 + 1) % 3 != 0) map (_._1)
  }

  drop(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j, 'k))

  // Problem 17 --Split a list into two parts.
  def split[T](n: Int, ls: List[T]): (List[T], List[T]) =
    (ls.take(n), ls.drop(n))

  split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

  // Problem 18 -- Extract a slice from a list.
  def slice[T](begin: Int, end: Int, ls: List[T]): List[T] =
    ls.drop(begin).take(end - (begin max 0))

  //  ls.zipWithIndex.filter(v=>v._2 >= begin && v._2 <= end).map(_._1)

  slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

  // Problem 19 -- Rotate a list N places to the left.
  def rotate[T](n: Int, ls: List[T]): List[T] = {
    val rotateVal = if (ls.isEmpty) 0 else n % ls.length
    if (rotateVal < 0) rotate(rotateVal + ls.length, ls)
    else
      ls.drop(rotateVal) ::: ls.take(rotateVal)
  }

  rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
}