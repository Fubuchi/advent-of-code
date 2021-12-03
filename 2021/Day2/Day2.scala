import scala.io.Source

@main def day2 =
    enum Direction:
        case Forward(value: Int)
        case Down(value: Int)
        case Up(value: Int)

    import Direction._

    def parse(text: String) =
        text.split(" ").toList match
            case "forward" :: v :: Nil => Forward(v.toInt)
            case "down" :: v :: Nil => Down(v.toInt)
            case "up" :: v :: Nil => Up(v.toInt)
            case _ => sys.error(s"Bad input: ${text}")

    def dive(acc: (Int, Int), curr: Direction) =
        val (hor, dep) = acc
        curr match
            case Forward(f) => (hor + f, dep)
            case Down(d)    => (hor, dep + d)
            case Up(u)      => (hor, dep - u)

    def diveAim(acc: (Int, Int, Int), curr: Direction) =
        val (hor, dep, aim) = acc
        curr match
            case Forward(f) => (hor + f, dep + f * aim, aim)
            case Down(d)    => (hor, dep, aim + d)
            case Up(u)      => (hor, dep, aim - u)

    val input = Source
        .fromFile("input.txt")
        .getLines
        .map(parse(_))
        .toList

    val (h1, d1) = input
        .foldLeft((0, 0))(dive)

    val (h2, d2, _) = input
        .foldLeft((0, 0, 0))(diveAim)

    println(s"Part 1: ${h1 * d1}")
    println(s"Part 2: ${h2 * d2}")
