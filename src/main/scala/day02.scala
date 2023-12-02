package aoc.day02

val CAPS = Map(
  "red" -> 12,
  "green" -> 13,
  "blue" -> 14
)

type Colour = String
case class Cube(color: Colour, count: Int)
case class Game(id: Int, hands: List[List[Cube]])

def parseCube(cube: String): Cube =
  cube match {
    case s"$value $name" => Cube(color = name, count = value.toInt)
  }

def parseHands(hands: String): List[List[Cube]] =
  hands.split("; ").toList.map(_.split(", ").toList.map(parseCube))

def parse(line: String): Game =
  line match {
    case s"Game $id: $hands" => Game(id = id.toInt, hands = parseHands(hands))
  }

def validateGame(game: Game): Boolean =
  game.hands.forall: hand =>
    hand.forall:
      case Cube(color, n) => n <= CAPS.get(color).get

def solve1(input: Iterator[String]): Int =
  input.map(parse).filter(validateGame).map(_.id).sum

def powerCubes(game: Game): Int =
  val acc: Map[String, Int] = Map.empty
  val maxima = game.hands.foldLeft(acc): (map, hand) =>
    hand.foldLeft(map): (map, cube) =>
      map.updatedWith(cube.color) {
        case Some(n) => Some(Math.max(n, cube.count))
        case None    => Some(cube.count)
      }

  maxima.values.product

def solve2(input: Iterator[String]): Int =
  input.map(parse andThen powerCubes).sum

@main def main: Unit =
  println(solve1(aoc.getInput(2)))
  println(solve2(aoc.getInput(2)))
