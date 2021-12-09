import scala.io.Source
import scala.annotation.tailrec

@main def day9 =
    type Point = (Int, Int)
    type Ocean = Vector[Vector[Int]]

    extension (ocean: Ocean) def |=>(cell: Point) = ocean(cell._2)(cell._1)

    def neighbors(X: Int, Y: Int)(cell: Point) =
        def move(from: Point)(dir: Point) =
            val (x, y) = from
            val (vx, vy) = dir
            (x + vx, y + vy)
        def inBound(cell: Point) =
            val (x, y) = cell
            !(x < 0 || x > X || y < 0 || y > Y)

        Vector((0, 1), (0, -1), (1, 0), (-1, 0))
            .map(move(cell))
            .filter(inBound)

    def isLow(ocean: Ocean)(cell: Point) =
        val X = ocean(0).length - 1
        val Y = ocean.length - 1
        val current = ocean |=> cell

        neighbors(X, Y)(cell)
            .forall(cell => (ocean |=> cell) > current)

    def lowest(ocean: Ocean) =
        val X = ocean(0).length - 1
        val Y = ocean.length - 1

        (for x <- 0 to X
        yield for y <- 0 to Y
        yield (x, y))
            .flatMap(identity)
            .filter(isLow(ocean))
            .toVector

    def firstPart(ocean: Ocean) =
        lowest(ocean)
            .map(cell => (ocean |=> cell) + 1)
            .sum

    def basin(ocean: Ocean)(cell: Point) =
        val X = ocean(0).length - 1
        val Y = ocean.length - 1

        def flood(visited: Set[Point], cell: Point): Set[Point] =
            if visited.contains(cell) || (ocean |=> cell) == 9 then visited
            else
                neighbors(X, Y)(cell)
                    .foldLeft(visited + cell)(flood)

        flood(Set(), cell).size

    def secondPart(ocean: Ocean) =
        lowest(ocean)
            .map(basin(ocean))
            .sortWith(_ > _)
            .take(3)
            .reduce(_ * _)

    val ocean = Source
        .fromFile("input.txt")
        .getLines
        .map(s => s.toCharArray().map(_.toString).map(_.toInt).toVector)
        .toVector

    println(s"First part: ${firstPart(ocean)}")
    println(s"Second part: ${secondPart(ocean)}")
