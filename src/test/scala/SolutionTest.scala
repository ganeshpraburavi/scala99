import collection.mutable.Stack
import org.scalatest._

class SolutionTest extends FlatSpec with Matchers {

  import Solution._

  it should "flatten a nested list struct" in {
    val list = List(List(1, 2, List(3, 4), List(2, 3), List(1), 2, 3))
    flatten2(list).toSeq should equal(List(1, 2, 3, 4, 2, 3, 1, 2, 3).toSeq)
  }

  it should "compress the list struct" in {
    val list = compress('Nothing, List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    list.toSeq should equal(List('a, 'b, 'c, 'a, 'd, 'e).toSeq)
  }

  it should "pack correctly" in {
    val list = pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    list.toSeq should equal(List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)).toSeq)
  }

  it should "encode correctly" in {
    val list = encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    list.toSeq should equal(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)).toSeq)
  }

  it should "encode modified correctly" in {
    val list = encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    list.toSeq should equal(List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e)).toSeq)
  }

  it should "decode correctly" in {
    val list = decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
    list.toSeq should equal(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e).toSeq)
  }

  it should "slice correctly" in {
    val list = slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    list.toSeq should equal(List('d, 'e, 'f, 'g).toSeq)
  }

  it should "rotate correctly" in {
    val list = rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    list.toSeq should equal(List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }

  it should "board should return neighbours correctly for a given cell" in {
    val sudokuInput = List(
      List(0, 0, 0, 2, 6, 0, 7, 0, 1),
      List(6, 8, 0, 0, 7, 0, 0, 9, 0),
      List(1, 9, 0, 0, 0, 4, 5, 0, 0),
      List(8, 2, 0, 1, 0, 0, 0, 4, 0),
      List(0, 0, 4, 6, 0, 2, 9, 0, 0),
      List(0, 5, 0, 0, 0, 3, 0, 2, 8),
      List(0, 0, 9, 3, 0, 0, 0, 7, 4),
      List(0, 4, 0, 0, 5, 0, 0, 3, 6),
      List(7, 0, 3, 0, 1, 8, 0, 0, 0))

    val firstRow = (1 until 9).map((0, _))
    val firstColumn = (1 until 9).map((_, 0))
    val firstBoxNeighbours = (0 until 3).flatMap(i => (0 until 3).map(j => (i, j))).filterNot(x => x._1 == 0 && x._2 == 0)
    val neighbours = (firstRow ++ firstColumn ++ firstBoxNeighbours).toSet
    val actualResult = Board(9, sudokuInput).neighbours(0, 0)
    (actualResult -- neighbours).size should be(0)
    (neighbours -- actualResult).size should be(0)
  }

  it should "isSolved return true" in {
    val sudokuInput = List(
      List(4, 3, 5, 2, 6, 9, 7, 8, 1),
      List(6, 8, 2, 5, 7, 1, 4, 9, 3),
      List(1, 9, 7, 8, 3, 4, 5, 6, 2),
      List(8, 2, 6, 1, 9, 5, 3, 4, 7),
      List(3, 7, 4, 6, 8, 2, 9, 1, 5),
      List(9, 5, 1, 7, 4, 3, 6, 2, 8),
      List(5, 1, 9, 3, 2, 6, 8, 7, 4),
      List(2, 4, 8, 9, 5, 7, 1, 3, 6),
      List(7, 6, 3, 4, 1, 8, 2, 5, 9))

    val cellInput = sudokuInput.map(x =>
      x.map(y => if (y == 0) NotAssumedCell(Set[Int](), Set[Int]()) else GivenCell(y, Set[Int](), Set[Int]())).to[IS]
    ).to[IS].asInstanceOf[IS[IS[Cell]]]

    Board.isSolved(9, cellInput) should be(true)
  }

  it should "board solve correctly" in {
    val sudokuInput = List(
      List(0, 0, 0, 2, 6, 0, 7, 0, 1),
      List(6, 8, 0, 0, 7, 0, 0, 9, 0),
      List(1, 9, 0, 0, 0, 4, 5, 0, 0),
      List(8, 2, 0, 1, 0, 0, 0, 4, 0),
      List(0, 0, 4, 6, 0, 2, 9, 0, 0),
      List(0, 5, 0, 0, 0, 3, 0, 2, 8),
      List(0, 0, 9, 3, 0, 0, 0, 7, 4),
      List(0, 4, 0, 0, 5, 0, 0, 3, 6),
      List(7, 0, 3, 0, 1, 8, 0, 0, 0))

    val solutions = Board(9, sudokuInput).solve
    if (solutions.size == 0)
      println("No Solutions found")
    else
      println("Number of Solutions: " + solutions.size)

    solutions.map(configuration => new Board(9, configuration)).map(println)

  }

}
