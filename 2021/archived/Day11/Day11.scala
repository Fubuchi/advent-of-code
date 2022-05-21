import scala.io.Source
import scala.annotation.tailrec

@main def day11 =
    type Point = (Int, Int)
    type Ocean = Vector[Vector[(Int, Boolean)]]

    extension (ocean: Ocean) def |=>(cell: Point) = ocean(cell._2)(cell._1)

    val X = 9
    val Y = 9

    def neighbors(cell: Point) =
        def move(from: Point)(dir: Point) =
            val (x, y) = from
            val (vx, vy) = dir
            (x + vx, y + vy)
        def inBound(cell: Point) =
            val (x, y) = cell
            !(x < 0 || x > X || y < 0 || y > Y)

        Vector(
          (0, 1),
          (0, -1),
          (1, 0),
          (-1, 0),
          (1, 1),
          (1, -1),
          (-1, 1),
          (-1, -1)
        )
            .map(move(cell))
            .filter(inBound)

    def flash(ocean: Ocean) =
        def waitingToFlash(ocean: Ocean) =
            (for x <- 0 to X
            yield for y <- 0 to Y
            yield (x, y))
                .flatMap(identity)
                .filter(cell =>
                    val (energy, flashed) = ocean |=> cell
                    energy > 9 && !flashed
                )
                .toSet

        @tailrec
        def flashing(toBeFlashed: Set[Point], ocean: Ocean): Ocean =
            toBeFlashed match
                case s if s.isEmpty => ocean
                case toBeFlashed =>
                    val nei =
                        toBeFlashed.map(neighbors(_)).toVector.flatMap(identity)

                    val next = ocean.zipWithIndex
                        .map((row, y) =>
                            row.zipWithIndex
                                .map((cell, x) =>
                                    val (e, f) = cell
                                    if toBeFlashed.contains((x, y)) then
                                        (e, true)
                                    else
                                        val plus = nei.count(_ == (x, y))
                                        (e + plus, f)
                                )
                        )
                    flashing(waitingToFlash(next), next)

        val flashed = ocean.map(_.map(cell => (cell._1 + 1, cell._2)))

        flashing(waitingToFlash(flashed), flashed)
            .map(_.map({ case (e, _) =>
                if e > 9 then (0, false) else (e, false)
            }))

    def countFlash(ocean: Ocean) =
        (for x <- 0 to X
        yield for y <- 0 to Y
        yield (x, y))
            .flatMap(identity)
            .count(cell => (ocean |=> cell)._1 == 0)

    def firstPart(ocean: Ocean) =
        (1 to 100)
            .foldLeft((0, flash(ocean)))((state, _) =>
                val (count, ocean) = state
                (count + countFlash(ocean), flash(ocean))
            )
            ._1

    def secondPart(ocean: Ocean) =
        @tailrec
        def loop(turn: Int, ocean: Ocean): Int =
            val count = countFlash(ocean)
            if count == (X + 1) * (Y + 1) then turn
            else loop((turn + 1), flash(ocean))

        loop(1, flash(ocean))

    val input = Source
        .fromFile("input.txt")
        .getLines
        .map(_.toCharArray.toVector.map(c => (c.toString.toInt, false)))
        .toVector

    println(s"First part: ${firstPart(input)}")
    println(s"Second part: ${secondPart(input)}")
