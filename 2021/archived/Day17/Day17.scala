import scala.io.Source
import scala.annotation.tailrec

@main def day17 =
    val abs = Math.abs
    val max = Math.max

    def boundX(x1: String, x2: String) =
        (abs(x1.toInt) :: abs(x2.toInt) :: Nil).sorted

    def boundY(y1: String, y2: String) =
        (y1.toInt :: y2.toInt :: Nil).sorted

    def inZone(minX: Int, maxX: Int, minY: Int, maxY: Int)(x: Int, y: Int) =
        minX <= x && x <= maxX && minY <= y && y <= maxY

    def shoot(maxX: Int, minY: Int, vx: Int, vy: Int) =
        @tailrec
        def loop(vx: Int, vy: Int, x: Int, y: Int, h: Int): (Int, Int, Int) =
            if x > maxX || y < minY then (x - vx, y - vy, h)
            else
                val nvx = max(vx - 1, 0)
                val nvy = vy - 1
                loop(nvx, nvy, x + nvx, y + nvy, max(h, y + nvy))

        loop(vx, vy, vx, vy, Int.MinValue)

    def multiShoot(minX: Int, maxX: Int, minY: Int, maxY: Int) =
        (for
            vx <- maxX to 0 by -1
            vy <- minY to abs(minY) - 1
        yield (vx, vy))
            .map({ case (vx, vy) =>
                shoot(maxX, minY, vx, vy)
            })
            .filter({ case (x, y, _) =>
                inZone(minX, maxX, minY, maxY)(x, y)
            })
            .toVector

    def firstPart(landing: Vector[(Int, Int, Int)]) =
        landing.maxBy(_._3)._3

    def secondPart(landing: Vector[(Int, Int, Int)]) = landing.size

    val raw = Source.fromFile("input.txt").getLines.next

    val (minX :: maxX :: Nil, minY :: maxY :: Nil) =
        "([0-9\\-]+)".r.findAllIn(raw).map(_.strip).toList match
            case x1 :: x2 :: y1 :: y2 :: Nil => (boundX(x1, x2), boundY(y1, y2))
            case _ => sys.error(s"Invalid input ${raw}")

    val landing = multiShoot(minX, maxX, minY, maxY)

    println(s"First part: ${firstPart(landing)}")
    println(s"Second part: ${secondPart(landing)}")
