package aoc.day15

type InitializationSeq = Seq[String]
type Lens = Int
type Time = Int
type Box = Map[String, (Lens, Time)]

def parse(input: Iterator[String]): InitializationSeq =
  input.next().split(',')

def hash(el: String): Int =
  el.foldLeft(0): (acc, c) =>
    ((acc + c.toInt) * 17) % 256

def solve1(input: Iterator[String]): Int =
  parse(input).map(hash).sum

def removeFromBox(
    boxes: Array[Box],
    label: String
): Array[Box] =
  val hashedLabel = hash(label)
  boxes.updated(hashedLabel, boxes(hashedLabel).removed(label))

def addToBox(
    boxes: Array[Box],
    label: String,
    lens: Lens,
    time: Time
): Array[Box] =
  val hashedLabel = hash(label)
  boxes.updated(
    hashedLabel,
    boxes(hashedLabel).updatedWith(label) {
      case None             => Some((lens, time))
      case Some(_, oldTime) => Some((lens, oldTime))
    }
  )

def solve2(input: Iterator[String]): Int =
  val boxes: Array[Box] = Array.fill(256)(Map.empty)
  val endState = parse(input).zipWithIndex
    .foldLeft(boxes): (map, elem) =>
      val (step, idx) = elem
      step match
        case s"$label-"      => removeFromBox(map, label)
        case s"$label=$lens" => addToBox(map, label, lens.toInt, idx)
    .zipWithIndex
    .filter((m, i) => !m.isEmpty)
    .map: (m, idx) =>
      (m.values.toSeq.sortBy(_._2).map(_._1), idx + 1)

  endState
    .map: (lenses, boxIdx) =>
      lenses.zipWithIndex.foldLeft(0): (acc, elem) =>
        val (lens, idx) = elem
        acc + boxIdx * (idx + 1) * lens
    .sum

@main def main: Unit =
  println(solve1(aoc.getInput(15)))
  println(solve2(aoc.getInput(15)))
