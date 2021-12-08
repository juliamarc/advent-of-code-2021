import scala.io.Source

object Day8 extends App {
    def decode(note: Array[Array[String]]) : Int = {
        val input = note(0).map(_.toSet)
        val output = note(1).map(_.toSet)
        var mapping = Map(
            input.filter(_.size == 2).head -> 1,
            input.filter(_.size == 3).head -> 7,
            input.filter(_.size == 4).head -> 4,
            input.filter(_.size == 7).head -> 8)
        val reverse_mapping = mapping.map(_.swap)

        for (pattern <- input.filter(p => List(5, 6).contains(p.size))) {
            val pattern_size = pattern.size
            val size_diff_to_1 = pattern.diff(reverse_mapping(1)).size
            val size_diff_to_4 = pattern.diff(reverse_mapping(4)).size

            if (pattern_size == 5) {
                if (size_diff_to_1 == 3) mapping += (pattern -> 3)
                else if (size_diff_to_4 == 3) mapping += (pattern -> 2)
                else mapping += (pattern -> 5)
            }
            else if (pattern_size == 6) {
                if (size_diff_to_1 == 5) mapping += (pattern -> 6)
                else if (size_diff_to_4 == 2) mapping += (pattern -> 9)
                else mapping += (pattern -> 0)
            }
        }

        return output.map(mapping(_)).mkString("").toInt
    }

    val notes = Source.fromFile("input.txt").getLines().
        map(_.split("\\s+\\|\\s+").map(_.split("\\s+"))).toList

    val count_part_one = notes.
        flatMap(_(1)).
        filter(p => List(2, 3, 4, 7).contains(p.length)).
        size
    println(s"Part One: ${count_part_one}")

    val sum_part_two = notes.map(decode(_)).reduce(_ + _)
    println(s"Part Two: ${sum_part_two}")
}
