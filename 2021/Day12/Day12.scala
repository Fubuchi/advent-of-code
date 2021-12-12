import scala.io.Source
import scala.util.chaining._
import scala.language.implicitConversions

@main def day12 =
    case class Route(
        val current: String,
        val visited: Set[String],
        val dupSmall: Option[String]
    )

    def extract(current: String, branch: (String, String)) =
        if branch._1 == current then branch._2
        else branch._1

    def addBranch(node: String, target: String)(map: Map[String, Set[String]]) =
        map.get(node) match
            case None    => map + (node -> Set(target))
            case Some(s) => map + (node -> (s + target))

    def isSmall(node: String) = node.toCharArray.forall(_.isLower)

    def endWith(node: String, branch: (String, String)) = branch._2 == node

    def startWith(node: String, branch: (String, String)) = branch._1 == node

    def goBack(map: Map[String, Set[String]], doubleVisit: Boolean)(
        route: Route
    ): Int =
        val Route(current, visited, dupSmall) = route
        (current, visited.contains(current)) match
            case ("start", _) => 1
            case ("end", _)   => 0
            case (node, true)
                if isSmall(node) && (!doubleVisit || !dupSmall.isEmpty) =>
                0
            case (node, seen) =>
                val newVisited = visited + current
                val newDupSmall = dupSmall match
                    case None =>
                        if seen && doubleVisit && isSmall(node) then Some(node)
                        else None
                    case s @ Some(_) => s

                map.get(current)
                    .get
                    .view
                    .map(next =>
                        goBack(
                          map,
                          doubleVisit
                        )(Route(next, newVisited, newDupSmall))
                    )
                    .sum

    def solve(doubleVisit: Boolean, input: List[(String, String)]) =
        val map = input.foldLeft(Map[String, Set[String]]())((m, branch) =>
            val (l, r) = branch
            m.pipe(addBranch(l, r)).pipe(addBranch(r, l))
        )

        input
            .filter(branch =>
                startWith("end", branch) || endWith("end", branch)
            )
            .map(branch => Route(extract("end", branch), Set("end"), None))
            .map(goBack(map, doubleVisit))
            .sum

    val input = Source
        .fromFile("input.txt")
        .getLines
        .map(s =>
            val Array(l, r) = s.split("-")
            (l, r)
        )
        .toList

    println(s"First part: ${solve(false, input)}")
    println(s"Second part: ${solve(true, input)}")
