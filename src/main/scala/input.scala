package aoc

import java.io._
import java.net.URI
import java.nio.file.Files
import java.nio.file.Paths
import scala.io.Source
import scala.sys
import scala.util.Properties
import scala.util.Using

def getInput(day: Int, example: Boolean = false): Iterator[String] =
  val fileName =
    f"src/main/resources/day$day%02d${if example then "_examples" else ""}.txt"

  if !Files.exists(Paths.get(fileName)) then fetchInput(day, fileName);
  Source.fromFile(fileName).getLines()

def fetchInput(day: Int, fileName: String): Unit =
  val url = URI.create(s"https://adventofcode.com/2023/day/$day/input").toURL
  val connection = url.openConnection()
  val sessionKey = sys.env.get("AOC_SESSION").get
  connection.addRequestProperty("Cookie", s"session=$sessionKey")

  Using.resources(
    connection.getInputStream,
    new FileWriter(fileName)
  ) { (input, writer) =>
    for line <- Source.fromInputStream(input) do writer.write(line)
  }
