import scala.io.Source
@main def day1 =
    val input = Source
        .fromFile("input.txt")
        .getLines
        .map(_.toInt)
        .toList

    val part1 = input
        .sliding(2)
        .map(pair => (pair(0), pair(1)))
        .count(_ < _)

    val part2 = input
        .sliding(3)
        .map(_.sum)
        .sliding(2)
        .map(pair => (pair(0), pair(1)))
        .count(_ < _)

    println(s"Part 1: ${part1}")
    println(s"Part 2: ${part2}")
