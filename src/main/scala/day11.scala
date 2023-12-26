package aoc.day11

type Position = (Int, Int)
type Galaxies = List[Position]

def parse(
    input: Iterator[String],
    factor: Int
): Galaxies =
  val grid = input.map(_.toList).toList

  val rowCumSizes = grid
    .map(row => if row.forall(_ == '.') then factor else 1)
    .scanLeft(0)(_ + _)
    .tail

  val colCumSizes = grid.transpose
    .map(col => if col.forall(_ == '.') then factor else 1)
    .scanLeft(0)(_ + _)
    .tail

  grid.zipWithIndex.flatMap:
    case (row, i) =>
      row.zipWithIndex.collect:
        case (element, j) if element == '#' =>
          (rowCumSizes(i), colCumSizes(j))

def manhattanDistance(
    from: Position,
    to: Position
): Long =
  (from._1 - to._1).abs + (from._2 - to._2).abs

def solve(input: Iterator[String], factor: Int): Long =
  parse(input, factor)
    .combinations(2)
    .map(c => manhattanDistance(c(0), c(1)))
    .sum

def solve1(input: Iterator[String]): Long =
  solve(input, 2)

def solve2(input: Iterator[String]): Long =
  solve(input, 1_000_000)

@main def main: Unit =
  println(solve1(aoc.getInput(11)))
  println(solve2(aoc.getInput(11)))
