import scala.io.Source
import scala.util.chaining._
import scala.language.implicitConversions
import scala.annotation.tailrec

@main def day18 =
    extension (s: String)
        def |(start: Int) = s.slice(start, s.length)
        def ||(start: Int, end: Int) = s.slice(start, end + 1)

    extension (v: Vector[(Int, Int)])
        def ^(start: Int) = v.slice(start, v.size)
        def ^^(start: Int, end: Int) = v.slice(start, end + 1)

    extension (t: (Int, Int))
        def value = t._1
        def depth = t._2

    def extractNumber(s: String) =
        @tailrec
        def loop(s: String, idx: Int): Int =
            idx match
                case i if i == s.length => i - 1
                case i =>
                    s(i) match
                        case n if n.isDigit => loop(s, i + 1)
                        case _              => i - 1

        loop(s, 0)

    def parse(s: String) =
        @tailrec
        def loop(
            s: String,
            numbers: List[(Int, Int)],
            depth: Int
        ): Vector[(Int, Int)] =
            s match
                case "" => numbers.reverse.toVector
                case s =>
                    s(0) match
                        case '[' => loop(s | 1, numbers, depth + 1)
                        case ']' => loop(s | 1, numbers, depth - 1)
                        case c if c.isDigit =>
                            val i = extractNumber(s)
                            val n = (s || (0, i)).toInt
                            loop(s | (i + 1), (n, depth) :: numbers, depth)
                        case _ => loop(s | 1, numbers, depth)

        loop(s, Nil, 0)

    def checkExplode(numbers: Vector[(Int, Int)]) =
        numbers.zipWithIndex.find(_._1.depth >= 5).map(_._2)

    def checkSplit(numbers: Vector[(Int, Int)]) =
        numbers.zipWithIndex.find(_._1.value >= 10).map(_._2)

    @tailrec
    def reduce(numbers: Vector[(Int, Int)]): Vector[(Int, Int)] =
        (checkExplode(numbers), checkSplit(numbers)) match
            case (Some(e), _) =>
                val limit = numbers.size - 2
                val ((lv, ld), (rv, rd)) = (numbers(e), numbers(e + 1))
                e match
                    case 0 =>
                        val (v, d) = numbers(2)
                        reduce(
                          Vector((0, ld - 1), (v + rv, d)) ++ (numbers ^ 3)
                        )
                    case i if i == limit =>
                        val (v, d) = numbers(limit - 1)
                        reduce(
                          (numbers ^^ (0, limit - 2)) ++ Vector(
                            (v + lv, d),
                            (0, rd - 1)
                          )
                        )
                    case i =>
                        val ((vl, dl), (vr, dr)) =
                            (numbers(i - 1), numbers(i + 2))
                        val left = (numbers ^^ (0, i - 2)) ++ Vector(
                          (vl + lv, dl),
                          (0, ld - 1)
                        )
                        val right = Vector((vr + rv, dr)) ++ (numbers ^ (i + 3))
                        reduce(left ++ right)
            case (_, Some(s)) =>
                val (v, d) = numbers(s)
                val left = (numbers ^^ (0, s - 1)) ++ Vector(
                  (v / 2, d + 1),
                  ((v + 1) / 2, d + 1)
                )
                val right = numbers ^ (s + 1)
                reduce(left ++ right)
            case _ => numbers

    def sum(left: Vector[(Int, Int)], right: Vector[(Int, Int)]) =
        (left ++ right)
            .map { case (v, d) => (v, d + 1) }
            .pipe(reduce)

    @tailrec
    def total(reduced: Vector[(Int, Int)]): Int =
        reduced match
            case Vector(v) => v.value
            case xs =>
                val maxDepth = xs.map(_.depth).max
                val i = xs.zipWithIndex
                    .find { case ((_, d), i) => d == maxDepth }
                    .map(_._2)
                    .get
                val left = xs ^^ (0, i - 1)
                val right = xs ^ (i + 2)
                val ((l, _), (r, _)) = (xs(i), xs(i + 1))
                total(left ++ Vector((3 * l + 2 * r, maxDepth - 1)) ++ right)

    def firstPart(numbers: Vector[Vector[(Int, Int)]]) =
        numbers
            .reduceLeft(sum)
            .pipe(total)

    def secondPart(numbers: Vector[Vector[(Int, Int)]]) =
        (for
            x <- 0 until numbers.size
            y <- 0 until numbers.size
            if x != y
        yield firstPart(Vector(numbers(x), numbers(y)))).max

    val math = Source
        .fromFile("input.txt")
        .getLines
        .map(parse)
        .toVector

    println(s"First part: ${firstPart(math)}")
    println(s"Second part: ${secondPart(math)}")
