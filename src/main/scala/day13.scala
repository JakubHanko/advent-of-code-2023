package aoc.day13

import scala.annotation.tailrec

type MirrorPattern = List[List[Char]]
type Vertical = Int
type Horizontal = Int

def parse(input: Iterator[String]): List[MirrorPattern] =
  if input.isEmpty then List()
  else
    input
      .takeWhile(!_.isBlank)
      .map(_.toList)
      .toList :: parse(input)

def countDifferences(left: MirrorPattern, right: MirrorPattern): Int =
  left
    .zip(right)
    .map((l, r) => l.zip(r).count(_ != _))
    .sum

def findReflection(mirror: MirrorPattern, wantedDifferences: Int): Int =
  @tailrec
  def compute(candidate: Int): Int =
    if candidate == mirror.size then 0
    else
      val splitSize = candidate min mirror.size - candidate
      val left = mirror.slice(candidate - splitSize, candidate)
      val right =
        mirror.slice(candidate, candidate + splitSize).reverse

      if countDifferences(left, right) == wantedDifferences then candidate
      else compute(candidate + 1)
  compute(1)

def getPointsOfIncidence(wantedDifferences: Int)(
    mirror: MirrorPattern
): (Vertical, Horizontal) =
  (
    findReflection(mirror.transpose, wantedDifferences),
    findReflection(mirror, wantedDifferences)
  )

def solve1(input: Iterator[String]): Int =
  parse(input).map(getPointsOfIncidence(0)).map(_ + 100 * _).sum

def solve2(input: Iterator[String]): Int =
  parse(input).map(getPointsOfIncidence(1)).map(_ + 100 * _).sum

@main def main: Unit =
  println(solve1(aoc.getInput(13)))
  println(solve2(aoc.getInput(13)))
