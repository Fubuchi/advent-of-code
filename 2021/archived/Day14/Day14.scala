import scala.io.Source

@main def day14 =
    def insert(seq: Map[String, Long], instructions: Map[String, String])(
        steps: Int
    ) =
        (1 to steps).foldLeft(seq)((state, _) =>
            state
                .map(freq =>
                    val (pair, count) = freq
                    val Array(l, r) = pair.toCharArray
                    instructions.get(pair) match
                        case Some(value) =>
                            Array((l + value, count), (value + r, count))
                        case _ => Array(freq)
                )
                .flatMap(identity)
                .groupBy(_._1)
                .mapValues(pair => pair.map(_._2).sum)
                .toMap
        )

    def solve(seq: String, instructions: Map[String, String])(steps: Int) =
        val last = seq.last
        val init = seq
            .sliding(2)
            .map(x => (x, 1L))
            .toVector
            .groupBy(_._1)
            .mapValues(pair => pair.map(_._2).sum).toMap

        val result = (insert(init, instructions)(steps))
            .groupBy(freq =>
                val (pair, count) = freq
                val Array(l, r) = pair.toCharArray
                l
            )
            .map((c, m) =>
                val s = m.map(_._2).sum
                if c == last then (c, s + 1) else (c, s)
            )
        val max = result.maxBy(_._2)._2
        val min = result.minBy(_._2)._2
        max - min

    val instructions = Source
        .fromFile("instructions.txt")
        .getLines
        .map(line =>
            val Array(l, r) = line.split(" -> ")
            (l, r)
        )
        .toMap

    val text = "CKKOHNSBPCPCHVNKHFFK"

    println(s"First part: ${solve(text, instructions)(10)}")
    println(s"Second part: ${solve(text, instructions)(40)}")
