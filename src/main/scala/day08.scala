package aoc.day08
import cats.implicits._

type LeftRight = (String, String)

def getInstructions(input: Iterator[String]): String =
  input.take(1).toList.head

def parseMaps(input: Iterator[String]): Map[String, LeftRight] =
  val map: Map[String, LeftRight] = Map.empty

  input.foldLeft(map): (map, line) =>
    line match {
      case s"$from = ($left, $right)" => map + (from -> (left, right))
    }

def solve1(input: Iterator[String]): Long =
  val instructions = getInstructions(input)
  val map = parseMaps(input.drop(1))

  // foldWhileLeft
  val path = Iterator
    .continually(instructions)
    .flatten
    .scanLeft("AAA"): (state, instr) =>
      if instr == 'L' then map(state)._1 else map(state)._2
    .takeWhile(state => state != "ZZZ")

  path.size

def gcd(a: Long, b: Long): Long = if (b == 0) a.abs else gcd(b, a % b)
def lcm(a: Long, b: Long): Long = (a.abs * b.abs) / gcd(a, b)

case class State(name: String, numberOfHits: Long, cycleLength: Option[Long])

// doesn't work in general case. Need to consider the case they are not already synchronised
def solve2(input: Iterator[String]): Long =
  val instructions = getInstructions(input)
  val map = parseMaps(input.drop(1))
  var state = map.keySet
    .filter(_.endsWith("A"))
    .map(s => State(name = s, numberOfHits = 0, cycleLength = None))
  println(state)

  val path = Iterator
    .continually(instructions)
    .flatten
    .scanLeft(state): (state, instr) =>
      state.map: (s) =>
        val newState = if instr == 'L' then map(s.name)._1 else map(s.name)._2
        val numberOfHits =
          if (newState.endsWith("Z")) s.numberOfHits + 1 else s.numberOfHits
        val cycleLength =
          if (numberOfHits == 1) s.cycleLength |+| Some(1) else s.cycleLength

        State(
          name = newState,
          numberOfHits = numberOfHits,
          cycleLength = cycleLength
        )
    .takeWhile(_.exists(state => state.numberOfHits < 2))

  path.toList.last.map(_.cycleLength).flatten.reduceLeft(lcm)

@main def main: Unit =
  println(solve1(aoc.getInput(8)))
  println(solve2(aoc.getInput(8)))
