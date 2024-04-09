package polyglot.a01b

import polyglot.OptionToOptional
import util.Optionals.Optional as ScalaOptional
import util.Sequences.Sequence
import java.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:

  trait Cell:
    type Position
    def position: Position
    def neighbours: Int
    def neighbours_= (numbers: Int): Unit

  private object Cell:
    private class CellImpl(private val x: Int, private val y: Int) extends  Cell:
      opaque type Position = (Int, Int)
      override def position: Position = (x, y)
      override var neighbours: Int = 0

      private def canEqual(other: Any): Boolean = other.isInstanceOf[CellImpl]
      override def equals(other: Any): Boolean = other match
        case that: CellImpl =>
          that.canEqual(this) &&
            x == that.x &&
            y == that.y
        case _ => false

      override def hashCode(): Int =
        val state = Seq(x, y)
        state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

    def apply(x: Int, y: Int): Cell = CellImpl(x, y)

  trait MinesGenerator:
    def generateRandomMines: Sequence[Cell]

  private object MinesGenerator:
    private class MinesGeneratorImpl(nMines: Int) extends MinesGenerator:
      import Sequence.*

      private val random = Random()
      private var mines: Sequence[Cell] = Sequence[Cell]()
      override def generateRandomMines: Sequence[Cell] =
        for i <- 0 until nMines do
          var cell = createRandomCell
          while mines.contains(cell) do
            cell = createRandomCell
          mines = Cons(cell, mines)
        mines

      private def createRandomCell: Cell =
        Cell(random.nextInt(size), random.nextInt(size))
    def apply(): MinesGenerator = MinesGeneratorImpl(mines)
  trait Grid:
    def addCell(cell: Cell): Unit
    def isMine(cell:Cell): Boolean
    def isFreeCellsFull: Boolean

  private object Grid:
    private class GridImpl extends Grid:
      private val minesSeq: Sequence[Cell] = MinesGenerator().generateRandomMines
      private var freeCells: Sequence[Cell] = Sequence[Cell]()

      import Sequence.*

      override def addCell(cell: Cell): Unit =
        cell.neighbours = minesSeq.filter(c => near(c, cell)).count()
        freeCells = Cons(cell, freeCells)

      private def near(cellToFind: Cell, cell: Cell): Boolean =
        var found: Boolean = false
        cellToFind.position match
          case (x: Int, y: Int) =>
            for i <- x-1 to x+1 do
              for j <- y-1 to y+1 do
                if (i < size && j < size && i >= 0 && j >= 0 && (i, j) == cell.position)
                  found = true
            found

      override def isMine(cell: Cell): Boolean = minesSeq.contains(cell)
      override def isFreeCellsFull: Boolean = (size*size) == freeCells.count() + mines

    def apply(): Grid = GridImpl()

  private val grid: Grid = Grid()

  override def hit(x: Int, y: Int): java.util.Optional[Integer] =
    val cell: Cell = Cell(x, y)
    OptionToOptional(
      if grid.isMine(cell) then
        ScalaOptional.Empty()
      else
        grid.addCell(cell)
        ScalaOptional.Just(cell.neighbours)
    )

  override def won: Boolean = grid.isFreeCellsFull