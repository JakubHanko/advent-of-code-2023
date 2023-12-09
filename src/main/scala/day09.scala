package aoc.day09

def parseLine(input: String): Array[Int] =
  input.split("\\s+").map(_.toInt)

def predictNextValue(values: Array[Int]): Int =
  if (values.forall(v => v == 0)) 0
  else {
    val diffs = values.sliding(2).map { case Array(x, y) => y - x }.toArray
    values.last + predictNextValue(diffs)
  }

def solve1(input: Iterator[String]): Int =
  input.map(parseLine andThen predictNextValue).sum

def solve2(input: Iterator[String]): Int =
  input.map(parseLine andThen (l => l.reverse) andThen predictNextValue).sum

@main def main: Unit =
  println(solve1(aoc.getInput(9)))
  println(solve2(aoc.getInput(9)))
