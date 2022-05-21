import scala.io.Source
import scala.util.chaining._
import scala.language.implicitConversions
import scala.annotation.tailrec

@main def day6 =
    def init(days: Array[Int]) =
        val fishes = for i <- 0 to 8 yield 0

        days
            .foldLeft(fishes)((fs, f) =>
                val curr = fs(f)
                fs.updated(f, curr + 1)
            )
            .map(_.toLong)
            .toVector

    def nextDay(fishes: Vector[Long]) =
        val additionalSix, nextEight = fishes(0)

        (fishes :+ nextEight)
            .sliding(2)
            .zipWithIndex
            .map((pair, day) =>
                val Vector(_, next) = pair
                if day == 6 then next + additionalSix
                else next
            )
            .toVector

    def solve(left: Int, fishes: Vector[Long]) =
        @tailrec
        def loop(left: Int, fishes: Vector[Long]): Vector[Long] =
            if left == 0 then fishes
            else loop(left - 1, nextDay(fishes))

        loop(left, fishes).sum

    val input = Source
        .fromFile("input.txt")
        .getLines
        .next
        .split(',')
        .map(_.toInt)
        .pipe(init)

    println(s"First part: ${solve(80, input)}")
    println(s"Second part: ${solve(256, input)}")
