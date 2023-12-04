package aoc.day04

case class GameResults(id: Int, matchesCount: Int)

def addPoints(results: GameResults): Int =
  math.pow(2, results.matchesCount - 1).toInt

def findMatches(winning: String, have: String): Seq[Int] =
  winning.strip.split("\\s+").map(_.toInt) intersect have.strip
    .split("\\s+")
    .map(_.toInt)

def parse(line: String): GameResults =
  line match {
    case s"Card $id: $winning | $have" =>
      GameResults(
        id = id.strip.toInt,
        matchesCount = findMatches(winning, have).length
      )
  }

def solve1(input: Iterator[String]): Int =
  input.map(parse andThen addPoints).sum

def solve2(input: Iterator[String]): Int =
  val cards = input.map(parse).toList
  val acc = Map.from((1 to cards.length).map(i => i -> 1))
  val result = cards
    .foldLeft(acc): (map, result) =>
      (result.id + 1 to Math.min(cards.length, result.id + result.matchesCount))
        .foldLeft(map): (map, id) =>
          map.updatedWith(id) {
            case Some(n) =>
              Some(n + map(result.id))
            case None => Some(1)
          }

  result.values.sum

@main def main: Unit =
  println(solve1(aoc.getInput(4)))
  println(solve2(aoc.getInput(4)))
