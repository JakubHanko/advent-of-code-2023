package aoc.day19

import scala.annotation.tailrec

type Label = String
type WorkFlow = Map[Label, Array[Rule]]
type Ranges = Map[Category, Range]

case class Part(values: Map[Category, Int]):
  def sum(): Int = values.values.sum
  def getValue(category: Category): Int = values(category)

enum Result:
  case Accept, Reject

object Result:
  def parse(input: String): Result | Label =
    input match
      case "A"   => Accept
      case "R"   => Reject
      case label => label

import Result._

enum Category:
  case X, M, A, S

import Category._

object Category:
  def parse(input: String): Category =
    input match
      case "x" => X
      case "m" => M
      case "a" => A
      case "s" => S

sealed trait Operator:
  def eval(a: Int, b: Int): Boolean

object LessThan extends Operator:
  def eval(a: Int, b: Int): Boolean = a < b

object MoreThan extends Operator:
  def eval(a: Int, b: Int): Boolean = a > b

sealed trait Rule
case class Terminal(target: Result | Label) extends Rule
case class Conditional(
    category: Category,
    value: Int,
    op: Operator,
    target: Terminal
) extends Rule

def parseWorkflows(input: Iterator[String]): WorkFlow =
  input
    .map: line =>
      line match
        case s"$label{$rules}" =>
          label -> rules
            .split(',')
            .map:
              case s"$cat<$value:$next" =>
                Conditional(
                  Category.parse(cat),
                  value.toInt,
                  LessThan,
                  Terminal(Result.parse(next))
                )
              case s"$cat>$value:$next" =>
                Conditional(
                  Category.parse(cat),
                  value.toInt,
                  MoreThan,
                  Terminal(Result.parse(next))
                )
              case s"$term" =>
                Terminal(Result.parse(term))
    .toMap

def parseParts(input: Iterator[String]): Iterator[Part] =
  input.map: line =>
    line match
      case s"{x=$x,m=$m,a=$a,s=$s}" =>
        Part(Map(X -> x.toInt, M -> m.toInt, A -> a.toInt, S -> s.toInt))

def parse(input: Iterator[String]): (WorkFlow, Iterator[Part]) =
  (parseWorkflows(input.takeWhile(!_.isBlank)), parseParts(input))

@tailrec
def flow(
    workflow: WorkFlow,
    part: Part,
    rules: Array[Rule]
): Option[Int] =
  rules.head match
    case Terminal(Accept)        => Some(part.sum())
    case Terminal(Reject)        => None
    case Terminal(label: String) => flow(workflow, part, workflow(label))
    case Conditional(category, value, op, target)
        if op.eval(part.getValue(category), value) =>
      flow(workflow, part, Array(target))
    case Conditional(_, _, _, _) =>
      flow(workflow, part, rules.tail)

def solve1(input: Iterator[String]): Int =
  val (workflow, parts) = parse(input)
  parts
    .map: part =>
      flow(workflow, part, workflow("in"))
    .flatten
    .sum

def splitRanges(
    ranges: Ranges,
    category: Category,
    op: Operator,
    pivot: Int
): (Ranges, Ranges) =
  val range = ranges(category)
  val (acceptRange, rejectRange) = op match
    case LessThan => ((range.start to pivot - 1), (pivot to range.end))
    case MoreThan => ((pivot + 1 to range.end), (range.start to pivot))

  (ranges.updated(category, acceptRange), ranges.updated(category, rejectRange))

def flowRanges(
    workflow: WorkFlow,
    ranges: Ranges,
    rules: Array[Rule]
): Long =
  rules.head match
    case Terminal(Accept) => ranges.values.map(_.size.toLong).product
    case Terminal(Reject) => 0
    case Terminal(label: String) =>
      flowRanges(workflow, ranges, workflow(label))
    case Conditional(category, value, op, target) =>
      val (acceptRanges, rejectRanges) =
        splitRanges(ranges, category, op, value)
      flowRanges(workflow, acceptRanges, Array(target))
        + flowRanges(workflow, rejectRanges, rules.tail)

def solve2(input: Iterator[String]): Long =
  val (workflow, _) = parse(input)
  val ranges = Map(
    X -> (1 to 4000),
    M -> (1 to 4000),
    A -> (1 to 4000),
    S -> (1 to 4000)
  )

  flowRanges(workflow, ranges, workflow("in"))

@main def main: Unit =
  println(solve1(aoc.getInput(19)))
  println(solve2(aoc.getInput(19)))
