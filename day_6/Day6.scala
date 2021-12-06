import scala.io.Source

object Day6 extends App {
    val fish = Source.fromFile("input.txt").getLines().next().split(",").map(_.toInt).toList
    val days_part_one = 80
    val days_part_two = 256

    var aging_fish = fish
    for (day <- 1 to days_part_one) {
        aging_fish = aging_fish.flatMap {
            case (0) => List(6, 8)
            case (f) => List(f - 1)
        }
    }
    println(s"Part One: ${aging_fish.size}")

    var aging_fish_counter = 1.to(8).
        map(a => (a, fish.count(_ == a))).
        map(a => (a._1, BigInt(a._2))).toList
    for (day <- 1 to days_part_two) {
        aging_fish_counter = aging_fish_counter.flatMap {
            case ((0, n)) => List((6, n), (8, n))
            case ((f, n)) => List((f - 1, n))
        }.
        groupBy(_._1).
        map { case (k, g) => (k, g.map(_._2).reduce(_ + _)) }.toList
    }
    println(s"Part Two: ${aging_fish_counter.map(_._2).reduce(_ + _)}")
}
