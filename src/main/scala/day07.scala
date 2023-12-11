package aoc.day07

import scala.collection.mutable.Map

enum HandType:
  case HighCard, OnePair, TwoPairs, ThreeOfAKind, FullHouse, FourOfAKind,
    FiveOfAKind

case class Hand(cards: String, bid: Int, handType: HandType, handStrength: Int)

case class Rules(cardOrder: String, joker: Boolean)

given handOrdering: Ordering[Hand] = (h1: Hand, h2: Hand) =>
  if (h1.handType != h2.handType) h1.handType.ordinal - h2.handType.ordinal
  else
    h1.handStrength - h2.handStrength

def evaluateCards(cards: String, rules: Rules): HandType =
  val jokerCount = cards.count(_ == 'J')
  var freqs = cards
    .filter(c => cards == "JJJJJ" || c != 'J')
    .groupBy(identity)
    .mapValues(_.length)
    .toMap

  val mostCommon = freqs.maxBy(_._2)._1
  if (rules.joker && mostCommon != 'J')
    freqs = freqs.updated(mostCommon, freqs(mostCommon) + jokerCount)
    freqs = freqs.removed('J')

  freqs.values.toList
    .sortWith(_ > _) match {
    case 5 :: _        => HandType.FiveOfAKind
    case 4 :: _        => HandType.FourOfAKind
    case 3 :: 2 :: Nil => HandType.FullHouse
    case 3 :: _        => HandType.ThreeOfAKind
    case 2 :: 2 :: _   => HandType.TwoPairs
    case 2 :: _        => HandType.OnePair
    case _             => HandType.HighCard
  }

def getHandStrength(cards: String, rules: Rules): Int =
  cards.zipWithIndex.foldLeft(0): (acc, card) =>
    val (c, i) = card
    rules.cardOrder.size * acc + (rules.cardOrder.indexOf(c))

def parse(rules: Rules)(line: String): Hand =
  line match {
    case s"$hand $bid" =>
      Hand(
        cards = hand,
        bid = bid.toInt,
        handType = evaluateCards(hand, rules),
        handStrength = getHandStrength(hand, rules)
      )
  }

def computeTotalWinnings(input: Iterator[String], rules: Rules): Int =
  input
    .map(parse(rules))
    .toList
    .sorted
    .zipWithIndex
    .map { (hand, rank) => (rank + 1) * hand.bid }
    .sum

def solve1(input: Iterator[String]): Int =
  computeTotalWinnings(input, Rules(cardOrder = "23456789TJQKA", joker = false))

def solve2(input: Iterator[String]): Int =
  computeTotalWinnings(input, Rules(cardOrder = "J23456789TQKA", joker = true))

@main def main: Unit =
  println(solve1(aoc.getInput(7)))
  println(solve2(aoc.getInput(7)))
