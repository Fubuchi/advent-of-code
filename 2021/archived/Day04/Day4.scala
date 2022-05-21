import scala.io.Source

import scala.util.chaining._
import scala.language.implicitConversions
import scala.annotation.tailrec

@main def day4 =
    type Board = Array[Array[(Int, Boolean, Int)]]
    type Row = Array[(Int, Boolean, Int)]
    type Col = Row

    val numbers = Source
        .fromFile("call.txt")
        .getLines
        .next
        .split(',')
        .map(_.toInt)
        .toList

    val boards = Source
        .fromFile("boards.txt")
        .getLines
        .filter(!_.isBlank)
        .grouped(5)
        .map(group =>
            group
                .map(
                  _.split(" +")
                      .filter(_.trim != "")
                      .map(n => (n.toInt, false, 0))
                )
                .filter(!_.isEmpty)
                .toArray
        )
        .toList

    def col(board: Board, index: Int) = board.map(_(index))
    def row(board: Board, index: Int) = board(index)

    def isWin(rowOrCol: Row | Col) =
        rowOrCol
            .forall(_._2)

    def isBoardWin(board: Board) =
        Range(0, 5, 1)
            .map(index =>
                col(board, index).pipe(isWin) || row(board, index).pipe(isWin)
            )
            .exists(identity)

    def winner(boards: List[Board]) =
        boards.find(isBoardWin) match
            case None => (boards, None)
            case win  => (boards.filter(!isBoardWin(_)), win)

    def mark(boards: List[Board], num: Int, time: Int) =
        for board <- boards
        yield for row <- board
        yield for cell <- row
        yield
            val (n, c, t) = cell
            if n == num then (n, true, time)
            else cell

    def sum(board: Board) =
        val lastCall =
            def checkWin(board: Board, direction: (Board, Int) => Row | Col) =
                Range(0, 5, 1)
                    .map(direction(board, _))
                    .find(isWin)
                    .map(_.maxBy(_._3))
                    .map(_._1)

            checkWin(board, row)
                .orElse(checkWin(board, col))
                .get

        (for
            row <- board
            cell <- row
        yield
            val (n, c, _) = cell
            if c then 0
            else n).sum * lastCall

    def firstPart(numbers: List[Int], boards: List[Board]) =
        @tailrec
        def loop(
            numbers: List[Int],
            time: Int,
            boards: List[Board],
            win: Option[Board]
        ): Board =
            win match
                case Some(win) => win
                case None =>
                    numbers match
                        case Nil => sys.error("No one win!")
                        case n :: ns =>
                            val (nextBoars, win) =
                                mark(boards, n, time).pipe(winner)
                            loop(ns, time + 1, nextBoars, win)

        loop(numbers, 1, boards, None).pipe(sum)

    def secondPart(numbers: List[Int], boards: List[Board]) =
        @tailrec
        def loop(
            numbers: List[Int],
            time: Int,
            boards: List[Board],
            lastWin: Option[Board]
        ): Board =
            boards match
                case Nil => lastWin.get
                case _ =>
                    numbers match
                        case Nil => lastWin.get
                        case n :: ns =>
                            val (nextBoars, win) =
                                mark(boards, n, time).pipe(winner)

                            loop(ns, time + 1, nextBoars, win.orElse(lastWin))

        loop(numbers, 1, boards, None).pipe(sum)

    println(s"Part 1: ${firstPart(numbers, boards)}")
    println(s"Part 1: ${secondPart(numbers, boards)}")
