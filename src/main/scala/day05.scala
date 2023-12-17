package aoc.day05

import scala.collection.immutable.NumericRange

type Seed = Long
type SeedRange = NumericRange[Seed]

def parseSeeds(line: String): Array[Seed] =
  line match {
    case s"seeds: $seeds" => seeds.trim.split("\\s+").map(_.toLong)
  }

case class Mapping(range: SeedRange, to: Seed) {
  def intersect(r: SeedRange): Boolean =
    range.contains(r.start) || range.contains(r.end)

  def getIntersect(r: SeedRange): Option[SeedRange] =
    val intersectStart = math.max(range.start, r.start)
    val intersectEnd = math.min(range.end, r.end)
    if (intersectStart < intersectEnd)
      Some(
        intersectStart until intersectEnd
      )
    else None

  def apply(n: Seed): Seed = to + n - range.start
  def apply(r: SeedRange): (Option[SeedRange], List[SeedRange]) =
    getIntersect(r) match
      case None => (None, List(r))
      case Some(intersection) =>
        val toRange =
          (to + intersection.head - range.start) until (to + intersection.last - range.start + 1)
        var res: List[SeedRange] = List()
        if (r.start < intersection.head)
          res = (r.start until intersection.head) :: res
        if (r.end > intersection.last + 1)
          res = (intersection.last + 1 until r.end) :: res

        (Some(toRange), res)
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
      .toList
      .sortBy(_.range.start) :: parseMaps(lines)

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

def splitRanges(
    ranges: (List[SeedRange], List[SeedRange]),
    map: Mapping
): (List[SeedRange], List[SeedRange]) =
  val (mapped, unmapped) = ranges
  val (newMapped, newUnmapped) =
    unmapped.map(map.apply(_)).unzip

  (mapped ++ newMapped.flatten, newUnmapped.flatten)

// TODO: refactor to use better abstractions when the AoC is over
def solve2(input: Iterator[String]): Seed =
  val (seeds, mappings) = parse(input)

  val seedRanges = createSeedRanges(seeds)
  val locations = mappings.foldLeft(seedRanges): (seedRanges, maps) =>
    maps
      .foldLeft((List(): List[SeedRange], seedRanges))(splitRanges)
      .toList
      .flatten

  locations.map(_.start).min

@main def main: Unit =
  println(solve1(aoc.getInput(5)))
  println(solve2(aoc.getInput(5)))
