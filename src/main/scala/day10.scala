package aoc.day10

import scala.annotation.tailrec

type Position = (Int, Int)

val VERTICAL_PIPE = '|'
val HORIZONTAL_PIPE = '-'
val NE_PIPE = 'L'
val NW_PIPE = 'J'
val SW_PIPE = '7'
val SE_PIPE = 'F'
val GROUND = '.'
val START = 'S'

case class Maze(grid: List[List[Char]], start: Position):
  def getNext(from: Position, prev: Position): Position =
    getAdjacencyList(from, grid(from._1)(from._2)).filter(_ != prev).head

  def getAdjacencyList(from: Position, tile: Char): List[Position] =
    val (x, y) = from

    tile match
      case VERTICAL_PIPE =>
        List((x - 1, y), (x + 1, y))
      case HORIZONTAL_PIPE =>
        List((x, y - 1), (x, y + 1))
      case NE_PIPE =>
        List((x, y + 1), (x - 1, y))
      case NW_PIPE =>
        List((x, y - 1), (x - 1, y))
      case SE_PIPE =>
        List((x + 1, y), (x, y + 1))
      case SW_PIPE =>
        List((x + 1, y), (x, y - 1))
      case START =>
        List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
          .filter((i, j) => grid.isDefinedAt(i) && grid(i).isDefinedAt(j))
          .collect {
            case p if getAdjacencyList(p, grid(p._1)(p._2)).contains(from) => p
          }
      case _ => List()

  def getTile(pos: Position): Char =
    val (x, y) = pos
    if pos != start then grid(x)(y)
    else
      // We don't know what is the real tile of the START. We try all possible border tiles.
      List(VERTICAL_PIPE, HORIZONTAL_PIPE, SE_PIPE, SW_PIPE, NE_PIPE, NW_PIPE)
        .map(t => t -> getAdjacencyList(pos, t).sorted)
        .filter((_, l) => l == getAdjacencyList(pos, START).sorted)
        .head
        ._1

def parseMaze(input: Iterator[String]): Maze =
  val grid = input.map(_.toList).toList
  val start = grid.zipWithIndex.collectFirst {
    case (row, rowIndex) if row.contains(START) =>
      (rowIndex, row.indexOf(START))
  }.get

  Maze(grid = grid, start = start)

def findLoop(maze: Maze): Set[Position] =
  @tailrec
  def traverse(
      current: Position,
      previous: Position,
      path: Set[Position]
  ): Set[Position] =
    if current != maze.start || path.isEmpty then
      traverse(maze.getNext(current, previous), current, path + current)
    else path

  traverse(maze.start, null, Set())

def solve1(input: Iterator[String]): Int =
  val maze = parseMaze(input)
  findLoop(maze).size / 2

def checkInRegion(inRegion: Boolean, char: Char): Boolean =
  inRegion ^ List(VERTICAL_PIPE, SE_PIPE, SW_PIPE).contains(char)

def scanLine(loop: Set[Position], maze: Maze)(line: List[Char], i: Int): Int =
  line.indices
    .foldLeft((false, 0)):
      case ((inRegion, count), j) if loop((i, j)) =>
        (checkInRegion(inRegion, maze.getTile((i, j))), count)
      case ((true, count), _)     => (true, count + 1)
      case ((inRegion, count), _) => (inRegion, count)
    ._2

def solve2(input: Iterator[String]): Int =
  val maze = parseMaze(input)
  val loop = findLoop(maze)

  maze.grid.zipWithIndex.map(scanLine(loop, maze)).sum

@main def main: Unit =
  println(solve1(aoc.getInput(10)))
  println(solve2(aoc.getInput(10)))
