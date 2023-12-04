package aoc.day03

type Row = IndexedSeq[Char]
type Matrix = IndexedSeq[Row]
case class Position(x: Int, y: Int)
case class SchematicNumber(yLower: Int, yUpper: Int, x: Int, value: Int)

def inBounds(x: Int, upperBound: Int): Boolean =
  x >= 0 && x < upperBound

def getNeighbors(input: Matrix, pos: Position): Set[Position] =
  Seq(-1, 0, 1)
    .flatMap(i => Seq(-1, 0, 1).map(j => (i, j)))
    .filter { case (i, j) =>
      (i != 0 || j != 0) && inBounds(pos.x + i, input.length) && inBounds(
        pos.y + j,
        input(0).length
      )
    }
    .map { case (i, j) => Position(pos.x + i, pos.y + j) }
    .toSet

def getNumberBound(input: Matrix, pos: Position, direction: Int): Int =
  val x = pos.x
  var y = pos.y
  val row = input(x)

  while (inBounds(y + direction, row.length) && row(y + direction).isDigit) do
    y += direction
  ;
  y

def extractNumber(input: Matrix, pos: Position): SchematicNumber =
  val lower = getNumberBound(input, pos, -1)
  val upper = getNumberBound(input, pos, 1)

  SchematicNumber(
    yLower = lower,
    yUpper = upper,
    x = pos.x,
    value = input(pos.x).slice(lower, upper + 1).mkString.toInt
  )

def isSymbol(c: Char): Boolean =
  !c.isDigit && c != '.'

def extractNumbers(input: Matrix, pos: Position): Set[SchematicNumber] =
  getNeighbors(input, pos)
    .filter { case Position(x, y) => input(x)(y).isDigit }
    .map(p => extractNumber(input, p))

def partNumber(input: Matrix, pos: Position): Set[SchematicNumber] =
  if (!isSymbol(input(pos.x)(pos.y))) {
    Set()
  } else {
    extractNumbers(input, pos)
  }

def gearNumber(input: Matrix, pos: Position): Set[SchematicNumber] =
  if (input(pos.x)(pos.y) != '*') {
    Set()
  } else {
    val numbers = extractNumbers(input, pos)
    if (numbers.size == 2) numbers else Set()
  }

// alternatively, approach with Vector.sliding(3)?
def scanSchematic(
    input: Matrix,
    op: (Matrix, Position) => Set[SchematicNumber]
): Seq[Set[SchematicNumber]] =
  for {
    x <- input.indices
    y <- input(0).indices
    numbers = op(input, Position(x = x, y = y))
    if !numbers.isEmpty
  } yield numbers

def solve1(input: Matrix): Int =
  scanSchematic(input, partNumber).distinct.flatten.map(s => s.value).sum

def solve2(input: Matrix): Int =
  scanSchematic(input, gearNumber).distinct.map(x => x.map(_.value).product).sum

@main def main: Unit =
  val input: Matrix = aoc.getInput(3).map(_.toIndexedSeq).toIndexedSeq
  println(solve1(input))
  println(solve2(input))
