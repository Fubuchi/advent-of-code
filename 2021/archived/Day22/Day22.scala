import scala.io.Source
import scala.util.chaining._
import scala.language.implicitConversions

@main def day22 =

    case class Point(x: Int, y: Int, z: Int)

    case class Operation(on: Boolean, min: Point, max: Point)

    def len(a: Int, b: Int) = Math.abs(a - b) + 1

    // https://developer.mozilla.org/en-US/docs/Games/Techniques/3D_collision_detection
    def aabb(a: Operation, b: Operation) =
        a.min.x <= b.max.x
            && a.max.x >= b.min.x
            && a.min.y <= b.max.y
            && a.max.y >= b.min.y
            && a.min.z <= b.max.z
            && a.max.z >= b.min.z

    def intersect(a: Operation, b: Operation, on: Boolean) =
        if aabb(a, b) then
            Some(
              Operation(
                on,
                Point(
                  Math.max(a.min.x, b.min.x),
                  Math.max(a.min.y, b.min.y),
                  Math.max(a.min.z, b.min.z)
                ),
                Point(
                  Math.min(a.max.x, b.max.x),
                  Math.min(a.max.y, b.max.y),
                  Math.min(a.max.z, b.max.z)
                )
              )
            )
        else None

    def volume(o: Operation) =
        len(o.min.x, o.max.x).toLong
            * len(o.min.y, o.max.y).toLong
            * len(o.min.z, o.max.z).toLong
            * (if o.on then 1L else -1L)

    def reboot(operations: Vector[Operation]) =
        operations
            .foldLeft(List.empty[Operation])((currentOps, o) =>
                val newOps =
                    currentOps
                        .map(op => intersect(op, o, !op.on))
                        .flatten

                if o.on then currentOps ++ (o :: newOps)
                else currentOps ++ newOps
            )

    def parse(s: String) =
        val pattern = "[0-9\\-]+".r

        val num =
            pattern
                .findAllIn(s)
                .map(_.toInt)
                .toVector

        Operation(
          s.contains("on"),
          Point(num(0), num(2), num(4)),
          Point(num(1), num(3), num(5))
        )

    def firstPart(operations: Vector[Operation]) =
        val limit = Operation(
          true,
          Point(-50, -50, -50),
          Point(50, 50, 50)
        )
        operations
            .map(o => intersect(o, limit, o.on))
            .flatten
            .pipe(reboot)
            .foldLeft(0L)(_ + volume(_))

    def secondPart(operations: Vector[Operation]) =
        reboot(operations)
            .foldLeft(0L)(_ + volume(_))

    val operations =
        Source.fromFile("input.txt").getLines.map(parse).toVector

    println(s"First part: ${firstPart(operations)}")
    println(s"Second part: ${secondPart(operations)}")
