package aoc.day06

def parse1(input: String): Array[Long] =
  input match {
    case s"Time: $times"         => times.trim.split("\\s+").map(_.toInt)
    case s"Distance: $distances" => distances.trim.split("\\s+").map(_.toInt)
  }

def parse2(input: String): Long =
  input match {
    case s"Time: $times"         => times.filterNot(_.isSpaceChar).toLong
    case s"Distance: $distances" => distances.filterNot(_.isSpaceChar).toLong
  }

// Solve the (t - x) * x > d equation
def countPossibilities(time: Long, distance: Long): Long =
  val td = time.toDouble
  val dd = distance.toDouble

  val discriminant = Math.sqrt(td * td - 4 * dd)
  val x1 = (td - discriminant) / 2
  val x2 = (td + discriminant) / 2

  (x2.floor - x1.ceil + 1).toLong

def solve1(input: Iterator[String]): Long =
  val (times, distances) = (parse1(input.next()), parse1(input.next()))
  times
    .zip(distances)
    .map(countPossibilities)
    .product

def solve2(input: Iterator[String]): Long =
  val (time, distance) = (parse2(input.next()), parse2(input.next()))

  countPossibilities(time, distance)

@main def main: Unit =
  println(solve1(aoc.getInput(6)))
  println(solve2(aoc.getInput(6)))
