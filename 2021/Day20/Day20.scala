import scala.io.Source
import scala.annotation.tailrec

@main def day20 =

    val unit =
        Vector(
          (-1, 1),
          (0, 1),
          (1, 1),
          (-1, 0),
          (0, 0),
          (1, 0),
          (-1, -1),
          (0, -1),
          (1, -1)
        )

    def oddInfinity(code: String) = code(0).toString

    def evenInfinity(code: String) =
        oddInfinity(code) match
            case "0" => "0"
            case "1" => code(511).toString

    def enhance(
        X: Int,
        Y: Int,
        code: String,
        pixels: Map[(Int, Int), String],
        steps: Int
    ) =
        val odd = oddInfinity(code)
        val even = evenInfinity(code)

        @tailrec
        def loop(
            X: Int,
            Y: Int,
            turn: Int,
            prev: Map[(Int, Int), String],
            lights: Int
        ): Int =
            if turn == steps then lights
            else
                val (pixels, lights) = (for
                    x <- -1 - turn to X + 1
                    y <- turn + 1 to -Y - 1 by -1
                yield (x, y))
                    .foldLeft((Map.empty[(Int, Int), String], 0))({
                        case ((pixels, lights), (x, y)) =>
                            val index = unit
                                .map(v =>
                                    val (vx, vy) = v
                                    prev.get((x + vx, y + vy)) match
                                        case None =>
                                            if turn % 2 == 0 then even
                                            else odd
                                        case Some(bit) => bit
                                )
                                .mkString

                            val bit = code(Integer.parseInt(index, 2)).toString
                            (pixels + ((x, y) -> bit), bit.toInt + lights)
                    })
                loop(X + 1, Y + 1, turn + 1, pixels, lights)

        loop(X, Y, 0, pixels, 0)

    val raw = Source.fromFile("input.txt").getLines.toVector

    val code = raw(0).replace('.', '0').replace('#', '1')

    val paint = raw.slice(2, raw.size)

    val X = paint(0).size - 1
    val Y = paint.size - 1

    val pixels =
        (for
            x <- 0 to X
            y <- 0 to Y
        yield ((x, -y), if paint(y)(x) == '.' then "0" else "1"))
            .foldLeft(Map.empty[(Int, Int), String])({ case (m, (k, v)) =>
                m + (k -> v)
            })

    println(s"First part: ${enhance(X, Y, code, pixels, 2)}")
    println(s"Second part: ${enhance(X, Y, code, pixels, 50)}")
