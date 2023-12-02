package aoc.day02

val RED_CAP = 12
val GREEN_CAP = 13
val BLUE_CAP = 14

def checkValidState(state: String): Boolean =
  state
    .split(",")
    .forall(x =>
      x.strip().split(" ") match {
        case Array(n, "green") => n.toInt <= GREEN_CAP
        case Array(n, "red")   => n.toInt <= RED_CAP
        case Array(n, "blue")  => n.toInt <= BLUE_CAP
      }
    )

def checkValidGame(line: String): Boolean =
  val gameStates = line.split(":")(1)
  gameStates.split(";").forall(checkValidState)

def solve1(input: Iterator[String]): Int =
  input.zipWithIndex.filter(x => checkValidGame(x._1)).map(_._2 + 1).sum

def powerCubes(line: String): Int =
  val gameStates = line.split(":")(1)
  gameStates
    .split(";")
    .map(x => {
      x.split(",")
        .map(x => {
          x.strip().split(" ") match {
            case Array(n, "green") => Array(n.toInt, 0, 0)
            case Array(n, "red")   => Array(0, n.toInt, 0)
            case Array(n, "blue")  => Array(0, 0, n.toInt)
          }
        })
        .reduce((x, y) => x.zip(y).map { case (x, y) => x + y })
    })
    .transpose
    .map(_.max)
    .reduceLeft((x, y) => x * y)

def solve2(input: Iterator[String]): Int =
  input.map(powerCubes).sum

@main def main: Unit =
  println(solve1(aoc.getInput(2)))
  println(solve2(aoc.getInput(2)))
