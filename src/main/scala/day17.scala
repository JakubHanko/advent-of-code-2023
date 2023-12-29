package aoc.day17

import scala.annotation.tailrec
import scala.collection.mutable.PriorityQueue

type HeatLoss = Int
type Grid = List[List[HeatLoss]]
type Position = (Int, Int)
type Direction = (Int, Int)
enum Orientation:
  case Vertical, Horizontal
type State = (HeatLoss, Position, Direction)

val START = (0, 0)

def parse(input: Iterator[String]): Grid =
  input.map(_.map(_.asDigit).toList).toList

def enqueueNext(
    grid: Grid,
    pq: PriorityQueue[State],
    state: State,
    direction: Direction,
    min: Int,
    max: Int
): Unit =
  val (curLoss, (x, y), _) = state
  val (dx, dy) = direction
  var lossAcc = curLoss
  for (d <- 1 to max) do
    val newX = x + d * dx
    val newY = y + d * dy

    if grid.isDefinedAt(newX) && grid(newX).isDefinedAt(newY) then
      lossAcc += grid(newX)(newY)
      if d >= min then pq.enqueue((lossAcc, (newX, newY), direction))

// (0, 8) - 31
def dijkstra(grid: Grid, min: Int, max: Int): Int =
  @tailrec
  def explore(
      pq: PriorityQueue[State],
      visited: Set[(Position, Orientation)],
      goal: Position
  ): Int =
    val state =
      pq.dequeue() // the PQ is always non-empty before we find the goal
    val (curLoss, curPos, (dx, dy)) = state
    val (x, y) = curPos
    val curOr =
      if dx == 0 then Orientation.Horizontal else Orientation.Vertical
    if curPos == goal then curLoss
    else if visited((curPos, curOr))
    then explore(pq, visited, goal)
    else
      enqueueNext(grid, pq, state, (dx.abs ^ 1, dy.abs ^ 1), min, max)
      enqueueNext(grid, pq, state, (-(dx.abs ^ 1), -(dy.abs ^ 1)), min, max)
      explore(pq, visited + ((curPos, curOr)), goal)

  val pq =
    PriorityQueue[State]()(Ordering.by(-_._1))
  pq.enqueue((0, START, (0, 1)))
  pq.enqueue((0, START, (1, 0)))
  val goal = (grid.size - 1, grid(0).size - 1)

  explore(pq, Set(), goal)

def solve1(input: Iterator[String]): Int =
  val grid = parse(input)

  dijkstra(grid, 1, 3)

def solve2(input: Iterator[String]): Int =
  val grid = parse(input)

  dijkstra(grid, 4, 10)

@main def main: Unit =
  println(solve1(aoc.getInput(17)))
  println(solve2(aoc.getInput(17)))
