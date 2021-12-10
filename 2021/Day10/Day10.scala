import scala.io.Source
import scala.annotation.tailrec

@main def day10 =
    def isClose = Set(')', ']', '}', '>').contains

    def score(symbol: Char) =
        Map((')', 3), (']', 57), ('}', 1197), ('>', 25137)).get(symbol).get

    def cost(symbol: Char) =
        Map((')', 1L), (']', 2L), ('}', 3L), ('>', 4L)).get(symbol).get

    def findClose(symbol: Char) =
        Map(('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')).get(symbol).get

    def analyze(state: (List[Char], Boolean), current: Char) =
        val (symbols, corrupted) = state
        if corrupted then state
        else
            symbols match
                case Nil =>
                    if isClose(current) then (current :: Nil, true)
                    else (current :: Nil, false)
                case s @ symbol :: symbols =>
                    current match
                        case ')' =>
                            if symbol == '(' then (symbols, false)
                            else (current :: s, true)
                        case ']' =>
                            if symbol == '[' then (symbols, false)
                            else (current :: s, true)
                        case '}' =>
                            if symbol == '{' then (symbols, false)
                            else (current :: s, true)
                        case '>' =>
                            if symbol == '<' then (symbols, false)
                            else (current :: s, true)
                        case _ => (current :: s, false)

    def repair(symbols: List[Char]) =
        @tailrec
        def loop(instructions: List[Char], symbols: List[Char]): List[Char] =
            symbols match
                case Nil => instructions
                case symbol :: symbols =>
                    loop((findClose(symbol) :: instructions), symbols)

        loop(Nil, symbols).reverse

    def firstPart(symbols: Vector[Array[Char]]) =
        symbols
            .map(_.foldLeft((Nil, false))(analyze))
            .filter(_._2)
            .map(r => score(r._1.head))
            .sum

    def secondPart(symbols: Vector[Array[Char]]) =
        val costs = symbols
            .map(_.foldLeft((Nil, false))(analyze))
            .filter(!_._2)
            .map(r =>
                repair(r._1)
                    .map(cost)
                    .foldLeft(0L)((total, cost) => total * 5L + cost)
            )
            .sortBy(identity)

        costs(costs.size / 2)

    val input =
        Source.fromFile("input.txt").getLines.map(_.toCharArray).toVector

    println(s"First part: ${firstPart(input)}")
    println(s"Second part: ${secondPart(input)}")
