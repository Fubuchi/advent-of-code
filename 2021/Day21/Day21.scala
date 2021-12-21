import scala.annotation.tailrec

@main def Day21 =
    def landing(x: Int) = (x - 1) % 10 + 1

    def roll(d: Int) = (d - 1) % 100 + 1

    @tailrec
    def practice(
        score1: Int,
        score2: Int,
        pos1: Int,
        pos2: Int,
        dice: Int,
        turn: Int
    ): Int =
        val npos1 =
            landing(
              pos1
                  + roll(dice)
                  + roll(dice + 1)
                  + roll(dice + 2)
            )

        val npos2 =
            landing(
              pos2
                  + roll(dice + 3)
                  + roll(dice + 4)
                  + roll(dice + 5)
            )

        (score1 + npos1 >= 1000, score2 + npos2 >= 1000) match
            case (true, _) => score2 * (turn + 3)
            case (_, true) => (score1 + npos1) * (turn + 6)
            case _ =>
                practice(
                  score1 + npos1,
                  score2 + npos2,
                  npos1,
                  npos2,
                  roll(dice + 6),
                  turn + 6
                )

    def enterUniversal(score1: Int, score2: Int, pos1: Int, pos2: Int) =

        val universals =
            Map((3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1))

        def loop(
            score1: Int,
            score2: Int,
            pos1: Int,
            pos2: Int,
            p1Turn: Boolean,
            unis: Long
        ): (Long, Long) =
            if score1 >= 21 then (unis, 0L)
            else if score2 >= 21 then (0L, unis)
            else
                (3 to 9).foldLeft(0L, 0L)({ case ((wins1, wins2), u) =>
                    val us = universals.get(u).get
                    val (w1, w2) =
                        if p1Turn then
                            val npos1 = landing(pos1 + u)
                            val nscore1 = score1 + npos1
                            loop(nscore1, score2, npos1, pos2, false, unis * us)
                        else
                            val npos2 = landing(pos2 + u)
                            val nscore2 = score2 + npos2
                            loop(score1, nscore2, pos1, npos2, true, unis * us)

                    (wins1 + w1, wins2 + w2)
                })
        loop(score1, score2, pos1, pos2, true, 1L).toList.max

    println(s"Frist part: ${practice(0, 0, 5, 9, 1, 0)}")
    println(
      s"Second part: ${enterUniversal(0, 0, 5, 9)}"
    )
