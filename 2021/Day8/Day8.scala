import scala.io.Source

@main def day8 =
    type Signal = Vector[(Vector[String], Vector[String])]
    val unique = Map((2, "1"), (4, "4"), (3, "7"), (7, "8"))

    def decode(
        unique: Map[Int, String],
        data: (Vector[String], Vector[String])
    ) =
        val (left, right) = data
        val one = (left ++ right).find(s => s.length == 2).get
        val four = (left ++ right).find(s => s.length == 4).get

        right
            .map(s =>
                unique.get(s.length) match
                    case Some(v) => v
                    case None =>
                        s.length match
                            case 5 =>
                                if (s intersect one).length == 2 then "3"
                                else if (s intersect four).length == 2 then "2"
                                else "5"
                            case 6 =>
                                if (s intersect four).length == 4 then "9"
                                else if (s intersect one).length == 2 then "0"
                                else "6"
            )
            .mkString
            .toInt

    def firstPart(input: Signal, unique: Map[Int, String]) =
        input
            .map((_, r) =>
                r.map(_.length)
                    .map(unique.get(_))
                    .filter(!_.isEmpty)
                    .size
            )
            .sum

    def secondPart(input: Signal, unique: Map[Int, String]) =
        input
            .map(decode(unique, _))
            .sum

    val input = Source
        .fromFile("input.txt")
        .getLines
        .map(s =>
            val Array(l, r) = s.split("\\|")
            (l.trim.split(" ").toVector, r.trim.split(" ").toVector)
        )
        .toVector

    println(s"First part: ${firstPart(input, unique)}")
    println(s"Second part: ${secondPart(input, unique)}")
