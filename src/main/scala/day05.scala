package aoc.day05

import scala.collection.immutable.NumericRange

type Seed = Long
type SeedRange = NumericRange[Seed]

def parseSeeds(line: String): Array[Seed] =
  line match {
    case s"seeds: $seeds" => seeds.trim.split("\\s+").map(_.toLong)
  }

case class Mapping(range: SeedRange, to: Seed) {
  def apply(n: Seed): Seed = to + n - range.start
  // TODO: split ranges if needed
  // TODO2: consider part one with ranges of size 1?
  def apply(r: SeedRange): List[SeedRange] = ???
}

def createMapping(values: Array[Seed]): Mapping =
  val to = values(0)
  val from = values(1)
  val size = values(2)

  Mapping(
    range = from until from + size,
    to = to
  )

// TODO: make it tailrec
def parseMaps(lines: Iterator[String]): List[List[Mapping]] =
  if (lines.isEmpty) List()
  else
    lines
      // we drop the map header
      .drop(1)
      .takeWhile(!_.isBlank)
      .map(_.split("\\s+").map(_.trim.toLong))
      .map(createMapping)
      .toList :: parseMaps(lines)

def parse(input: Iterator[String]): (Array[Seed], List[List[Mapping]]) =
  (parseSeeds(input.take(1).toList.head), parseMaps(input.drop(1)))

def solve1(input: Iterator[String]): Seed =
  val (seeds, mappings) = parse(input)

  val locations = mappings
    .foldLeft(seeds): (seeds, maps) =>
      seeds.map: (seed) =>
        maps.find(_.range.contains(seed)).map(_.apply(seed)).getOrElse(seed)

  locations.min

def createSeedRanges(seeds: Array[Seed]): List[SeedRange] =
  seeds
    .grouped(2)
    .map { case Array(from, size) =>
      from until from + size
    }
    .toList

// TODO: refactor to use better abstractions when the AoC is over
def solve2(input: Iterator[String]): Seed =
  val (seeds, mappings) = parse(input)

  val seedRanges = createSeedRanges(seeds)
  val locations = mappings.foldLeft(seedRanges): (seedRanges, maps) =>
    maps.foldLeft(seedRanges): (seedRanges, map) =>
      seedRanges.map(range => map.apply(range)).flatten
      ???
      ???

  ???

@main def main: Unit =
  println(solve1(aoc.getInput(5)))
  println(solve2(aoc.getInput(5, true)))
