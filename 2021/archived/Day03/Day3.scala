import scala.io.Source
import scala.annotation.tailrec

@main def day3 =
    extension (s: String) def bin = Integer.parseInt(s, 2)

    val input = Source
        .fromFile("input.txt")
        .getLines
        .map(_.toList)
        .toList

    val init = List.fill(input.head.size)((0, 0))

    def sum(data: List[List[Char]]) =
        data.foldLeft(init)((state, curr) =>
            state
                .zip(curr)
                .map((state, current) =>
                    val (one, zero) = state
                    current match
                        case '0' => (one, zero + 1)
                        case '1' => (one + 1, zero)
                        case _   => sys.error(s"Bad input ${current}")
                )
        )

    def max(data: (Int, Int)) =
        val (one, zero) = data
        if one >= zero then '1' else '0'

    def min(data: (Int, Int)) =
        val (one, zero) = data
        if one >= zero then '0' else '1'

    def maxMin(data: (Int, Int)) = (max(data), min(data))

    def firstPart(input: List[List[Char]]) =
        val (max, min) = sum(input)
            .map(maxMin)
            .foldLeft(("", ""))((state, curr) =>
                val (sMax, sMin) = state
                val (cMax, cMin) = curr
                (sMax + cMax, sMin + cMin)
            )

        max.bin * min.bin

    def secondPart(input: List[List[Char]]) =
        def analyze(
            input: List[List[Char]],
            choose: ((Int, Int)) => Char,
            index: Int
        ) =
            sum(input).map(choose)(index)

        @tailrec
        def loop(
            choose: ((Int, Int)) => Char,
            index: Int,
            acc: List[List[Char]]
        ): List[List[Char]] =
            acc match
                case x :: Nil => x :: Nil
                case xs =>
                    val search = analyze(xs, choose, index)
                    val next = xs.filter(_(index) == search)
                    loop(choose, index + 1, next)

        val ma = loop(max, 0, input).head.mkString.bin
        val mi = loop(min, 0, input).head.mkString.bin

        ma * mi

    println(s"Part 1: ${firstPart(input)}")
    println(s"Part 1: ${secondPart(input)}")
