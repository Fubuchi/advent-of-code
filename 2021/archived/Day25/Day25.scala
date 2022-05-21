import scala.io.Source
import scala.annotation.tailrec

@main def day25 =
    def goEast(X: Int)(cucumber: (Int, Int)) =
        val (x, y) = cucumber
        if x + 1 > X then (0, y) else (x + 1, y)

    def goSouth(Y: Int)(cucumber: (Int, Int)) =
        val (x, y) = cucumber
        if y + 1 > Y then (x, 0) else (x, y + 1)

    def parse(X: Int, Y: Int, ocean: Vector[String]) =
        val (east, south) = (for
            x <- 0 to X
            y <- 0 to Y
        yield (x, y))
            .foldLeft(List.empty[(Int, Int)], List.empty[(Int, Int)])({
                case ((east, south), (x, y)) =>
                    ocean(y)(x) match
                        case '>' => ((x, y) :: east, south)
                        case 'v' => (east, (x, y) :: south)
                        case _   => (east, south)
            })

        (east.toSet, south.toSet)

    def firstPart(
        X: Int,
        Y: Int,
        east: Set[(Int, Int)],
        south: Set[(Int, Int)]
    ) =
        val goE = goEast(X)
        val goS = goSouth(Y)

        @tailrec
        def loop(
            east: Set[(Int, Int)],
            south: Set[(Int, Int)],
            turn: Int,
            stop: Boolean
        ): Int =
            if stop then turn
            else
                val toEast =
                    east.filter(cucumber =>
                        val next = goE(cucumber)
                        !east.contains(next) && !south.contains(next)
                    )
                val movede = toEast.map(goE)
                val neast = east.union(movede).diff(toEast)

                val toSouth =
                    south.filter(cucumber =>
                        val next = goS(cucumber)
                        !neast.contains(next) && !south.contains(next)
                    )

                val moveds = toSouth.map(goS)
                val nsouth = south.union(moveds).diff(toSouth)

                loop(neast, nsouth, turn + 1, toEast.isEmpty && toSouth.isEmpty)
        loop(east, south, 0, false)

    val raw = Source.fromFile("input.txt").getLines.toVector

    val X = raw(0).size - 1
    val Y = raw.size - 1

    val (east, south) = parse(X, Y, raw)

    println(s"First part: ${firstPart(X, Y, east, south)}")
    println(s"Second part: Merry Xmas")
