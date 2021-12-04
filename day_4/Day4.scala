import scala.io.Source

object Day4 extends App {
    val bingo = Source.fromFile("input.txt").getLines().toArray
    val numbers = bingo.head.split(",")
    val boards = bingo.
        tail.
        grouped(6).
        map(b => b.tail.map(r => r.replaceAll("\\s+", " ").trim.split(" "))).
        toArray

    var first_winning_board, first_winning_number, first_winning_board_sum = -1
    var last_winning_board, last_winning_number, last_winning_board_sum  = -1

    var winning_boards = Array[Int]()
    var active_boards = boards.zipWithIndex

    for (number <- numbers) {
        active_boards = active_boards.
            filterNot(b => winning_boards.contains(b._2)).
            map(b => (b._1.map(r => r.map(e => if(e == number) null else e)), b._2)).toArray

        val horizontal_wins = active_boards.
            map(b => (b._1.map(r => r.forall(_ == null)), b._2)).
            map(b => (b._1.exists(_ == true), b._2)).
            filter(b => b._1 == true).
            map(b => b._2).toArray

        val vertical_wins = active_boards.
            map(b => (b._1.transpose.map(r => r.forall(_ == null)), b._2)).
            map(b => (b._1.exists(_ == true), b._2)).
            filter(b => b._1 == true).
            map(b => b._2).toArray

        val new_winning_boards = horizontal_wins ++ vertical_wins

        if (new_winning_boards.length > 0) {
            winning_boards = winning_boards ++ new_winning_boards

            if (first_winning_board == -1) {
                first_winning_board = new_winning_boards.head
                first_winning_number = number.toInt
                first_winning_board_sum = active_boards.
                    find(b => b._2 == first_winning_board).
                    map(b => b._1).
                    head.
                    map(r => r.map(e => if (e == null) 0 else e.toInt)).
                    map(r => r.foldLeft(0)(_ + _)).
                    foldLeft(0)(_ + _)
            }
            last_winning_board = new_winning_boards.head
            last_winning_number = number.toInt
            last_winning_board_sum = active_boards.
                find(b => b._2 == last_winning_board).
                map(b => b._1).
                head.
                map(r => r.map(e => if (e == null) 0 else e.toInt)).
                map(r => r.foldLeft(0)(_ + _)).
                foldLeft(0)(_ + _)
        }
    }

    println(s"Part One: ${first_winning_board_sum * first_winning_number}")
    println(s"Part Two: ${last_winning_board_sum * last_winning_number}")
}
