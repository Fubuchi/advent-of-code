import scala.io.Source

@main def day5 =
    type Point = (Int, Int)
    type Start = Point
    type End = Point
    type Solution = ((Start, End)) => List[Point]

    def range(a: Int, b: Int) =
        if a > b then a to b by -1
        else a to b

    def straight(start: Start, end: End) =
        val ((x1, y1), (x2, y2)) = (start, end)
        (
          for x <- range(x1, x2)
          yield for y <- range(y1, y2)
          yield (x, y)
        ).flatMap(_.toList).toList

    def diagonal(start: Start, end: End) =
        val ((x1, y1), (x2, y2)) = (start, end)
        (range(x1, x2) zip range(y1, y2)).toList

    def firstPart(start: Start, end: End) =
        val ((x1, y1), (x2, y2)) = (start, end)
        if x1 != x2 && y1 != y2 then Nil
        else straight(start, end)

    def secondPart(start: Start, end: End) =
        val ((x1, y1), (x2, y2)) = (start, end)
        if x1 != x2 && y1 != y2 then diagonal(start, end)
        else straight(start, end)

    def input = Source
        .fromFile("input.txt")
        .getLines
        .map(_.split(" -> "))
        .map(line =>
            val Array(x1, y1) = line(0).split(",").map(_.toInt)
            val Array(x2, y2) = line(1).split(",").map(_.toInt)
            ((x1, y1), (x2, y2))
        )
        .toList

    def solve(input: List[(Start, End)], solution: Solution) =
        input
            .map(solution)
            .flatMap(identity)
            .groupBy(identity)
            .view
            .mapValues(_.size)
            .toMap
            .count(_._2 > 1)

    println(s"First part: ${solve(input, firstPart)}")
    println(s"Second part: ${solve(input, secondPart)}")
