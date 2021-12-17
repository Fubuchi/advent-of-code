import scala.io.Source
import scala.collection.immutable.SortedSet
import scala.annotation.tailrec

@main def day15 =
    type Node = (Int, Int)
    type Table = Vector[Vector[Int]]
    type Risk = Map[(Int, Int), Int]
    type Cost = Map[(Int, Int), Int]
    type PQueue = SortedSet[(Int, (Int, Int))]

    def inBound(X: Int, Y: Int)(p: Node) =
        val (x, y) = p
        !(x < 1 || x > X || y < 1 || y > Y)

    def neighbors(X: Int, Y: Int, p: Node) =
        val (x, y) = p
        Set((0, 1), (0, -1), (1, 0), (-1, 0))
            .map((vx, vy) => (x + vx, y + vy))
            .filter(inBound(X, Y))

    def risk(X: Int, Y: Int, tableX1: Table)(p: Node) =
        val (x, y) = p
        if x <= X && y <= Y then tableX1(y - 1)(x - 1)
        else
            val (modX, modY) = (x % X, y % Y)
            val (quotientX, quotientY) = (x / X, y / Y)
            val baseX = if modX == 0 then X else modX
            val baseY = if modY == 0 then Y else modY
            val baseEdge = tableX1(baseY - 1)(baseX - 1)

            val jumpX =
                (modX, quotientX) match
                    case (0, q) => q - 1
                    case (_, q) => q

            val jumpY =
                (modY, quotientY) match
                    case (0, q) => q - 1
                    case (_, q) => q

            (baseEdge + jumpX + jumpY) % 9 match
                case 0 => 9
                case e => e

    def dijkstra(X: Int, Y: Int, risks: Risk, source: Node, destination: Node) =
        def evaluate(
            evaluated: Set[Node],
            costs: Cost,
            node: Node,
            queue: PQueue
        ) =
            val fromCost = costs.get(node).get

            neighbors(X, Y, node)
                .filter(!evaluated.contains(_))
                .foldLeft((costs, queue))((state, node) =>
                    val (c, q) = state
                    val currentCost = c.get(node).get
                    val risk = risks.get(node).get

                    if risk + fromCost < currentCost then
                        (
                          c + (node -> (risk + fromCost)),
                          q + ((risk + fromCost, node))
                        )
                    else (c, q)
                )

        @tailrec
        def checkCost(
            costs: Cost,
            evaluating: PQueue,
            evaluated: Set[Node]
        ): Cost =
            (evaluating, evaluated.contains(destination)) match
                case (e, _) if e.isEmpty => costs
                case (_, true)           => costs
                case (eval, _) =>
                    val ((_, min), rest) = (eval.head, eval.tail)
                    val (c, q) = evaluate(evaluated, costs, min, rest)
                    checkCost(c, q, evaluated + min)

        val costs = risks.map {
            case ((1, 1), _) => ((1, 1) -> 0)
            case kv          => (kv._1 -> Int.MaxValue)
        }

        checkCost(costs, SortedSet((0, source)), Set())

    val table =
        Source
            .fromFile("input.txt")
            .getLines
            .map(s => s.toCharArray.map(_.toString.toInt).toVector)
            .toVector

    val Y = table.length

    val X = table(0).length

    val X5 = X * 5
    val Y5 = Y * 5

    val risks =
        (for
            x <- 1 to X
            y <- 1 to Y
        yield (x, y) -> table(y - 1)(x - 1)).toMap

    val risksX5 =
        (for
            x <- 1 to X5
            y <- 1 to Y5
        yield (x, y) -> risk(X, Y, table)((x, y))).toMap

    println(
      s"First part: ${dijkstra(X, Y, risks, (1, 1), (X, Y)).get((X, Y)).get}"
    )
    println(
      s"Second part: ${dijkstra(X5, Y5, risksX5, (1, 1), (X5, Y5)).get((X5, Y5)).get}"
    )
