import scala.io.Source

@main def day13 =
    def fold(
        instructions: Vector[((Int, Int), (Int, Int))],
        source: Set[(Int, Int)]
    ) =
        instructions.foldLeft(source)((s, pair) =>
            val (target, source) = pair
            if s.contains(source) then s - source + target
            else s
        )

    def foldX(Y: Int)(col: Int, source: Set[(Int, Int)]) =
        val newOld =
            (for x <- col + 1 to 2 * col
            yield for y <- 0 to Y
            yield ((x - 2 * (x - col), y), (x, y)))
                .flatMap(identity)
                .toVector

        fold(newOld, source)

    def foldY(X: Int)(line: Int, source: Set[(Int, Int)]) =
        val newOld =
            (for x <- 0 to X
            yield for y <- line + 1 to 2 * line
            yield ((x, y - 2 * (y - line)), (x, y)))
                .flatMap(identity)
                .toVector

        fold(newOld, source)

    def firstPart(X: Int, Y: Int)(
        instruction: (String, Int),
        source: Set[(Int, Int)]
    ) =
        instruction match
            case ("x", col)  => foldX(Y)(col, source).size
            case ("y", line) => foldY(X)(line, source).size

    def secondPart(X: Int, Y: Int)(
        instructions: Vector[(String, Int)],
        source: Set[(Int, Int)]
    ) =
        val result = instructions.foldLeft(source)((p, ins) =>
            ins match
                case ("x", col)  => foldX(Y)(col, p)
                case ("y", line) => foldY(X)(line, p)
        )

        val left = result.view.map(x => x._1).minBy(identity)
        val right = result.view.map(x => x._1).maxBy(identity)

        val top = result.view.map(x => x._2).minBy(identity)
        val bottom = result.view.map(x => x._2).maxBy(identity)

        (for y <- top to bottom
        yield for x <- left to right
        yield if result.contains((x, y)) then "0" else " ")
            .map(_.mkString)
            .foreach(println)

    val paper =
        Source
            .fromFile("paper.txt")
            .getLines
            .map(s =>
                val Array(l, r) = s.split(",")
                (l.toInt, r.toInt)
            )
            .toSet

    val instructions =
        Source
            .fromFile("fold.txt")
            .getLines
            .map(s =>
                val Array(l, r) = s.split("=")
                (l, r.toInt)
            )
            .toVector

    val X = paper.view.map(x => x._1).maxBy(identity)

    val Y = paper.view.map(x => x._2).maxBy(identity)

    println(s"First part: ${firstPart(X, Y)(instructions(0), paper)}")
    println("Second part:")
    secondPart(X, Y)(instructions, paper)
