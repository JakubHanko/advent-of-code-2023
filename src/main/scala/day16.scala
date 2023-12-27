package aoc.day16

import scala.annotation.tailrec
import scala.collection.mutable.Queue
import scala.collection.mutable.Set

type Grid = List[String]
type Position = (Int, Int)
type State = (Position, Orientation)

val EMPTY = '.'
val L_MIRROR = '/'
val R_MIRROR = '\\'
val V_SPLITTER = '|'
val H_SPLITTER = '-'

val START = (0, 0)
val INITIAL_ORIENTATION = Orientation.East

enum Orientation:
  case East, South, West, North

  // S to W, W to S, E to N, N to E
  def handleRMirror(): Orientation =
    ordinal match
      case 0 => Orientation.South
      case 1 => Orientation.East
      case 2 => Orientation.North
      case 3 => Orientation.West

  def handleLMirror(): Orientation =
    ordinal match
      case 0 => Orientation.North
      case 1 => Orientation.West
      case 2 => Orientation.South
      case 3 => Orientation.East

  def isVertical(): Boolean =
    ordinal % 2 == 1

  def isHorizontal(): Boolean =
    !isVertical()

  def getVertical(): List[Orientation] =
    List(Orientation.North, Orientation.South)

  def getHorizontal(): List[Orientation] =
    List(Orientation.West, Orientation.East)

def getNextPosition(pos: Position, orientation: Orientation): Position =
  val (x, y) = pos
  orientation match
    case Orientation.East  => (x, y + 1)
    case Orientation.West  => (x, y - 1)
    case Orientation.North => (x - 1, y)
    case Orientation.South => (x + 1, y)

def getNextOrientation(
    tile: Char,
    orientation: Orientation
): List[Orientation] =
  tile match
    case EMPTY    => List(orientation)
    case L_MIRROR => List(orientation.handleLMirror())
    case R_MIRROR => List(orientation.handleRMirror())
    case V_SPLITTER if orientation.isVertical()   => List(orientation)
    case V_SPLITTER                               => orientation.getVertical()
    case H_SPLITTER if orientation.isHorizontal() => List(orientation)
    case H_SPLITTER                               => orientation.getHorizontal()

def getEnergised(
    grid: Grid,
    startPos: Position,
    initialOrientation: Orientation
): Int =
  @tailrec
  def explore(
      queue: Queue[State],
      visited: Set[State],
      energised: Set[Position]
  ): Unit =
    if queue.isEmpty then ()
    else
      val (pos, orientation) = queue.dequeue
      val (x, y) = pos

      if !grid.isDefinedAt(x) || !grid(x).isDefinedAt(y) || visited(
          (pos, orientation)
        )
      then explore(queue, visited, energised)
      else
        energised += pos
        visited += ((pos, orientation))
        getNextOrientation(grid(x)(y), orientation).foreach: (newOr) =>
          val newPos = getNextPosition(pos, newOr)
          queue.enqueue((newPos, newOr))

        explore(queue, visited, energised)

  val energised: Set[Position] = Set()
  explore(Queue((startPos, initialOrientation)), Set(), energised)
  energised.size

def parse(input: Iterator[String]): Grid =
  input.toList

def solve1(input: Iterator[String]): Int =
  val grid = parse(input)
  getEnergised(grid, START, INITIAL_ORIENTATION)

def solve2(input: Iterator[String]): Int =
  val grid = parse(input)
  val verticalCandidates = (0 until grid(0).size).flatMap: i =>
    List(
      ((0, i), Orientation.South),
      ((grid(0).size - 1, i), Orientation.North)
    )
  val horizontalCandidates = (0 until grid.size)
    .flatMap: i =>
      List(
        ((i, 0), Orientation.East),
        ((grid.size - 1, i), Orientation.West)
      )

  (verticalCandidates ++ horizontalCandidates)
    .map: (pos, orientation) =>
      getEnergised(grid, pos, orientation)
    .max

@main def main: Unit =
  println(solve1(aoc.getInput(16)))
  println(solve2(aoc.getInput(16)))
