import scala.io.Source

@main def day7 =
    extension (crabs: Map[Int, Int])
        def |=>(target: Int)(
            fuel: Int => (Int, (Int, Int)) => Int
        ) =
            crabs
                .foldLeft(0)(fuel(target))

    val abs = Math.abs

    def progression(a: Int, b: Int) =
        (1 + abs(a - b)) * (abs(a - b)) / 2

    def firstPart(target: Int)(fuel: Int, crab: (Int, Int)) =
        val (pos, count) = crab
        fuel + abs(pos - target) * count

    def secondPart(target: Int)(fuel: Int, crab: (Int, Int)) =
        val (pos, count) = crab
        fuel + progression(pos, target) * count

    val raw = Source
        .fromFile("input.txt")
        .getLines
        .next
        .split(',')
        .map(_.toInt)
        .toVector

    val crabs = raw.groupBy(identity).view.mapValues(_.size).toMap

    val positions = raw.min to raw.max

    val first = positions
        .map(pos => (crabs |=> pos)(firstPart))
        .minBy(identity)

    val second = positions
        .map(pos => (crabs |=> pos)(secondPart))
        .minBy(identity)

    println(s"First part: ${first}")
    println(s"Second part: ${second}")
