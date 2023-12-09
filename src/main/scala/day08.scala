package aoc.day08
import cats.implicits._

type Options = (String, String)

def getInstructions(input: Iterator[String]): String =
  input.take(1).toList.head

def parseMaps(input: Iterator[String]): Map[String, Options] =
  val map: Map[String, Options] = Map.empty

  input.foldLeft(map): (map, line) =>
    line match {
      case s"$from = ($left, $right)" => map + (from -> (left, right))
    }

// TODO: doesn't work in general case when the cycles are not synchronised from the get go
def countSteps(
    instructions: String,
    map: Map[String, Options],
    endCondition: String => Boolean
)(start: String): Long =
  // foldWhileLeft
  val path = LazyList
    .continually(instructions)
    .flatten
    .scanLeft(start): (state, instr) =>
      if instr == 'L' then map(state)._1 else map(state)._2
    .takeWhile(state => !endCondition(state))

  path.size

def solve1(input: Iterator[String]): Long =
  val instructions = getInstructions(input)
  val map = parseMaps(input.drop(1))

  countSteps(instructions, map, _ == "ZZZ")("AAA")

def gcd(a: Long, b: Long): Long = if (b == 0) a.abs else gcd(b, a % b)
def lcm(a: Long, b: Long): Long = (a.abs * b.abs) / gcd(a, b)

def solve2(input: Iterator[String]): Long =
  val instructions = getInstructions(input)
  val map = parseMaps(input.drop(1))
  var states = map.keySet
    .filter(_.endsWith("A"))

  states.map(countSteps(instructions, map, _.endsWith("Z"))).reduceLeft(lcm)

@main def main: Unit =
  println(solve1(aoc.getInput(8)))
  println(solve2(aoc.getInput(8)))
