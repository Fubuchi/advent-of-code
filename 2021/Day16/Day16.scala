import scala.io.Source
import scala.annotation.tailrec

@main def day16 =

    extension (s: String)
        def |(start: Int) = s.slice(start, s.length)
        def ||(start: Int, end: Int) = s.slice(start, end + 1)

    enum Packet:
        case Literal(version: Int, value: Long)
        case Operation(version: Int, option: Int, subs: List[Packet])

    import Packet._

    val hex2bin = Map(
      ('0', "0000"),
      ('1', "0001"),
      ('2', "0010"),
      ('3', "0011"),
      ('4', "0100"),
      ('5', "0101"),
      ('6', "0110"),
      ('7', "0111"),
      ('8', "1000"),
      ('9', "1001"),
      ('A', "1010"),
      ('B', "1011"),
      ('C', "1100"),
      ('D', "1101"),
      ('E', "1110"),
      ('F', "1111")
    )

    val endPattern = "^[0]+$".r

    def endString(s: String) =
        endPattern.pattern.matcher(s).matches
            || s.length == 0

    def version(s: String) = Integer.parseInt(s || (0, 2), 2)

    def packageType(s: String) = Integer.parseInt(s || (3, 5), 2)

    def operation(s: String) = s(6).toString.toInt

    def literalLast(s: String) =
        @tailrec
        def loop(s: String, i: Int): Int =
            if s(i) == '0' then i + 4
            else loop(s, i + 5)

        loop(s, 0)

    def totalVersion(pkg: Packet) =
        def loop(pkg: Packet, acc: Int): Int =
            pkg match
                case Literal(v, _) => acc + v
                case Operation(v, _, ps) =>
                    acc + v + ps.foldLeft(0)((a, p) => a + loop(p, 0))

        loop(pkg, 0)

    def eval(pkg: Packet): Long =
        pkg match
            case Literal(_, value) => value
            case Operation(_, t, l) =>
                t match
                    case 0 => l.foldLeft(0L)((acc, p) => acc + eval(p))
                    case 1 => l.foldLeft(1L)((acc, p) => acc * eval(p))
                    case 2 => l.map(eval).min
                    case 3 => l.map(eval).max
                    case 5 =>
                        l match
                            case s :: f :: Nil =>
                                if eval(f) > eval(s) then 1 else 0
                            case _ => sys.error("Invalid type 5 payload")
                    case 6 =>
                        l match
                            case s :: f :: Nil =>
                                if eval(f) < eval(s) then 1 else 0
                            case _ => sys.error("Invalid type 6 payload")
                    case 7 =>
                        l match
                            case s :: f :: Nil =>
                                if eval(f) == eval(s) then 1 else 0
                            case _ => sys.error("Invalid type 7 payload")

    def buildPackage(s: String) =
        def literal(s: String) =
            val (v, idx) = (version(s), literalLast(s | 6))

            val value =
                for i <- 0 to idx if i % 5 != 0
                yield s(i + 6)

            (
              6 + idx + 1,
              Literal(v, java.lang.Long.parseLong(value.mkString, 2))
            )

        def loop(
            s: String,
            packets: List[Packet],
            limit: Option[Int]
        ): (String, List[Packet]) =
            (endString(s), limit) match
                case (true, _) | (_, Some(0)) => (s, packets)
                case _ =>
                    val (v, t, o) = (version(s), packageType(s), operation(s))
                    val lim = limit.map(_ - 1)
                    (t, o) match
                        case (4, _) =>
                            val (next, lit) = literal(s)
                            loop(s | next, lit :: packets, lim)
                        case (_, 0) =>
                            val range = Integer.parseInt(s || (7, 21), 2)

                            val (_, inner) =
                                loop(s || (22, 22 + range - 1), Nil, None)
                            val (next, other) = loop(s | (22 + range), Nil, lim)
                            (next, other ++ (Operation(v, t, inner) :: packets))
                        case _ =>
                            val innerL = Integer.parseInt(s || (7, 17), 2)

                            val (n, inner) = loop(s | 18, Nil, Some(innerL))
                            val (next, other) = loop(n, Nil, lim)
                            (next, other ++ (Operation(v, t, inner) :: packets))

        loop(s, Nil, Some(1))

    val input =
        Source
            .fromFile("input.txt")
            .getLines
            .next
            .toCharArray
            .map(hex2bin.get(_).get)
            .mkString

    val packet = buildPackage(input)._2.head

    println(s"First part: ${totalVersion(packet)}")
    println(s"Second part: ${eval(packet)}")
