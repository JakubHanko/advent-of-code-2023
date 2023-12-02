package aoc.day01

def calibrationValue(line: String): Int =
  val digits = line.collect { case c if c.isDigit => c.asDigit }

  digits.head * 10 + digits.last

def getDigit(idx: Int): Int =
  idx % 9 + 1

def calibrationValueLetters(line: String): Int =
  val candidates =
    Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
      ++ Range.inclusive(1, 9).map(_.toString)

  val first = candidates.zipWithIndex
    .minBy(x => {
      line.indexOf(x._1) match {
        case -1 => Int.MaxValue
        case i  => i
      }
    })
    ._2

  val last = candidates.zipWithIndex
    .maxBy(x => line.lastIndexOf(x._1))
    ._2

  getDigit(first) * 10 + getDigit(last)

def solve1(input: Iterator[String]): Int =
  input.map(calibrationValue).sum

def solve2(input: Iterator[String]): Int =
  input.map(calibrationValueLetters).sum

@main def main: Unit =
  println(solve1(aoc.getInput(1)))
  println(solve2(aoc.getInput(1)))
