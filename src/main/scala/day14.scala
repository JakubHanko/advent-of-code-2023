package aoc.day14

type Plan = List[String]

val BOULDER = 'O'
val EMPTY = '.'
val CUBE = '#'

def parse(input: Iterator[String]): Plan =
  input.toList

def countLoad(plan: Plan): Int =
  plan.transpose
    .map: (row) =>
      row
        .prepended(CUBE)
        .zipWithIndex
        .foldRight((0, Map.empty: Map[Int, Int])): (elem, acc) =>
          val (count, boulders) = acc
          val (tile, idx) = elem
          tile match
            case CUBE    => (0, boulders.updated(idx, count))
            case BOULDER => (count + 1, boulders)
            case EMPTY   => (count, boulders)
        ._2
        .filter(_._2 != 0)
        .toList
        .map: (boulderMap) =>
          val (place, count) = boulderMap
          val a = plan.size - place - count
          val b = a + count + 1

          (a + b) * count / 2
    .flatten
    .sum

def solve1(input: Iterator[String]): Int =
  (countLoad compose parse)(input)

def solve2(input: Iterator[String]): Int =
  ???

@main def main: Unit =
  println(solve1(aoc.getInput(14))) // 108935 - 8 ms
  println(solve2(aoc.getInput(14, true)))
