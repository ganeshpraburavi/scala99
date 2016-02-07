import scala.collection.immutable.::

object Solution {

  def last[T](list: List[T]): T = {
    if (list.isEmpty) throw new NoSuchElementException
    list match {
      case tail :: Nil => tail
      case head :: tail => last(tail)
    }
  }

  def penultimate[T](list: List[T]): T = {
    if (list.size < 2) throw new NoSuchElementException
    list match {
      case tailBefore :: tail :: Nil => tail
      case head :: tail => penultimate(tail)
    }
  }

  def nth[T](k: Int, list: List[T]): T = {
    list match {
      case head :: tail if k != 0 => nth(k - 1, tail)
      case head :: tail if k == 0 => head
      case Nil => throw new NoSuchElementException
    }
  }

  def reverse[T](list: List[T]): List[T] = {
    list match {
      case head :: tail => reverse(tail) ::: List(head)
      case Nil => Nil
    }
  }

  def tailReverse[T](list: List[T]): List[T] = {
    def util(result: List[T], ls: List[T]): List[T] = {
      ls match {
        case Nil => result
        case head :: tail => util(head :: result, tail)
      }
    }
    util(Nil, list)
  }

  def isPalindrome[T](list: List[T]): Boolean = {
    val rls = tailReverse(list)
    val result = rls.zip(list).foldLeft(true)((result, ele) => ele match {
      case (e1, e2) => result && e1 == e2
    })
    result
  }

  def flatten2(list: List[Any]): List[Any] = {
    list.foldLeft(List[Any]())((acc, ele) => {
      ele match {
        case ls: List[_] => acc ::: flatten2(ls)
        case ele => acc ::: List(ele)
      }
    })
  }


  def compress[T](lastSeen: T, list: List[T]): List[T] = {
    val (lseen, compressed) = list.foldLeft((lastSeen, List[T]())) { case ((lseen, acc), ele) => if (lseen == ele) (ele, acc) else (ele, acc ::: List(ele)) }
    compressed
  }

  // Ugly one but iterative. Suggestions welcome
  def pack[T](list: List[T]): List[List[T]] = {
    def packUtil(result: List[List[T]], ls: List[T]): List[List[T]] = {
      ls match {
        case head :: tail => val (pre, post) = ls span (_ == head); packUtil(pre :: result, post)
        case Nil => result
      }
    }
    packUtil(Nil, list).reverse
  }

  def encode[T](list: List[T]) = {
    pack(list).map(ele => (ele.size, ele.head))
  }

  def encodeModified[T](list: List[T]) = {
    pack(list).map(ele => if (ele.size > 1) (ele.size, ele.head) else ele.head)
  }

  def decode[T](list: List[(Int, T)]) = {
    list.flatMap(ele => List.fill(ele._1)(ele._2))
  }

  def encodeDirect[T](list: List[T]) = {
    def encodeUtil(result: List[(Int, T)], ls: List[T]): List[(Int, T)] = {
      ls match {
        case head :: tail => val (pre, post) = ls span (_ == head); encodeUtil((pre.size, pre.head) :: result, post)
        case Nil => result
      }
    }
    encodeUtil(List[(Int, T)](), list)
  }

  def duplicateN[T](n: Int, list: List[T]): List[T] = {
    list.flatMap(ele => List.fill(n)(ele))
  }

  def duplicate[T](list: List[T]): List[T] = {
    duplicateN(2, list)
  }

  def drop[T](n: Int, list: List[T]): List[T] = {
    list.zipWithIndex filter { v => (v._2 + 1) % n != 0 } map {
      _._1
    }
  }

  def split[T](n: Int, list: List[T]): (List[T], List[T]) = {
    (list.take(n), list.drop(n))
  }

  def slice[T](st: Int, end: Int, list: List[T]) = {
    list.zipWithIndex.filter { case (ele, idx) => idx >= st && idx < end }.map(_._1)
  }

  def rotate[T](n: Int, list: List[T]) = {
    val rotSize = (if (n < 0) n + list.size else n) % list.size
    list.drop(rotSize) ::: slice(0, rotSize, list)
  }

  def removeAt[T](n: Int, list: List[T]) = {

  }

  type IS[T] = scala.collection.mutable.IndexedSeq[T]

  class Cell(var value: Int, var restricted: Set[Int], var temporarilyRestricted: Set[Int])

  case class NotAssumedCell(restrictedSet: Set[Int], temporarilyRestrictedSet: Set[Int]) extends Cell(0, restrictedSet, temporarilyRestrictedSet)

  case class GivenCell(valueInt: Int, restrictedSet: Set[Int], temporarilyRestrictedSet: Set[Int]) extends Cell(valueInt, restrictedSet, temporarilyRestrictedSet)

  case class AssumedCell(valueInt: Int, restrictedSet: Set[Int], temporarilyRestrictedSet: Set[Int]) extends Cell(valueInt, restrictedSet, temporarilyRestrictedSet)

  class Board(N: Int, var board: IS[IS[Cell]]) {

    import Board._

    val possibles: Set[Int] = (1 to N).toSet[Int]

    def neighbours(x: Int, y: Int) = {
      def boxNeighbours: IS[(Int, Int)] = {
        val (bs, be) = ((x / 3) + 1, (y / 3) + 1)
        (bs * 3 - 3 until bs * 3).flatMap(r => (be * 3 - 3 until be * 3).map(c => (r, c))).filter { case (r, c) => r != x || c != y }.to[IS]
      }
      def rowNeighbours = (0 until N).filter(_ != x).map((_, y))
      def colNeighbours = (0 until N).filter(_ != y).map((x, _))
      (rowNeighbours ++ colNeighbours ++ boxNeighbours).toSet
    }

    def fillRestrictedValues = {
      for (x <- 0 until N;
           y <- 0 until N) {
        board(x)(y) match {
          case NotAssumedCell(_, temporarilyRestrictedSet) => {
            val restrictedSet = neighbours(x, y)
              .filter { case (nx, ny) =>
              board(nx)(ny) match {
                case NotAssumedCell(_, _) => false
                case _ => true
              }
            }.map { case (tx, ty) => board(tx)(ty).value }

            board(x)(y) = NotAssumedCell(restrictedSet, temporarilyRestrictedSet)
          }
          case _ =>
        }
      }
    }

    def addRestrictionToNeighbours(x: Int, y: Int, value: Int) = {
      neighbours(x, y)
        .map { case (nx, ny) => board(nx)(ny) match {
        case NotAssumedCell(resSet, tempRest) => board(nx)(ny) = NotAssumedCell(resSet, tempRest + value)
        case _ =>
      }
      }
    }

    def removeRestrictionToNeighbours(x: Int, y: Int, value: Int) = {
      neighbours(x, y)
        .map { case (nx, ny) => board(nx)(ny) match {
        case NotAssumedCell(resSet, tempRest) => board(nx)(ny) = NotAssumedCell(resSet, tempRest - value)
        case AssumedCell(valu, resSet, tempRest) => board(nx)(ny) = AssumedCell(valu, resSet, tempRest - value)
        case _ =>
      }
      }
    }

    def solve: List[IS[IS[Cell]]] = {
      var solutions = List[IS[IS[Cell]]]()
      val MAX_SOLUTION_SIZE = 10

      object NoPossibleException extends Exception {}
      object SolutionLimitExceededException extends Exception {}

      def solverUtil(modBoard: IS[IS[Cell]]): Unit = {
        if (isSolved(N, modBoard)) {
          val cloned = modBoard.map(_.clone().to[IS]).clone().to[IS]
          solutions = cloned :: solutions
        }
        try {
          modBoard.zipWithIndex.map { case (row, i) => row.zipWithIndex.map { case (ele, j) => ele match {
            case NotAssumedCell(restricted, temporarilyRestricted) =>
              val allPossibles = possibles -- (restricted ++ temporarilyRestricted)
              if (allPossibles.size == 0) throw NoPossibleException
              //if ((possibles -- restricted).size == 1) board(i)(j) = GivenCell((possibles -- restricted).head, Set[Int](), Set[Int]())
              allPossibles.map {
                assumeValue => {
                  if (solutions.size > 0) throw SolutionLimitExceededException
                  modBoard(i)(j) = AssumedCell(assumeValue, restricted, temporarilyRestricted)
                  addRestrictionToNeighbours(i, j, assumeValue)
                  solverUtil(modBoard)
                  removeRestrictionToNeighbours(i, j, assumeValue)
                  modBoard(i)(j) = NotAssumedCell(restricted, temporarilyRestricted)
                }
              }
            case _ =>
          }
          }
          }
        }
        catch {
          case NoPossibleException => return
          case SolutionLimitExceededException => return
        }
      }
      fillRestrictedValues
      solverUtil(board)
      solutions
    }

    override def toString() = {
      var newBuilder = new StringBuilder()
      val border = (1 to 36).map(i => if (i % 4 == 0) "-" else " ").mkString
      newBuilder.append(border + "\n")
      board.map(x => {
        newBuilder.append(" | ")
        x.map(y => newBuilder.append(y.value + " | "))
        newBuilder.append(s"\n$border\n")
      })
      newBuilder.toString()
    }
  }

  object Board {
    def apply(n: Int, input: List[List[Int]]): Board = {
      val cellInput = input.map(x =>
        x.map(y => if (y == 0) NotAssumedCell(Set[Int](), Set[Int]()) else GivenCell(y, Set[Int](), Set[Int]())).to[IS]
      ).to[IS].asInstanceOf[IS[IS[Cell]]]
      new Board(n, cellInput)
    }

    def isSolved(N: Int, modBoard: IS[IS[Cell]]): Boolean = {
      def isRowsSolved: Boolean = modBoard.map(_.filter(_.value != 0).groupBy(_.value).size == N).reduce(_ & _)
      def isColsSolved: Boolean = (0 until N).map(i => (0 until N).map(j => modBoard(j)(i))).map(column => column.filter(_.value != 0).groupBy(_.value).size == N).reduce(_ & _)
      isRowsSolved & isColsSolved
    }
  }

}
