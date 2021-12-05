import scala.io.Source

object Day5 extends App {
    def get_points_between(p1: Array[Int], p2:Array[Int]) : Array[Array[Int]] = {
        val (x1, y1, x2, y2) = (p1(0), p1(1), p2(0), p2(1))

        if (x1 == x2) {
            return y1.to(y2).by((y2 - y1).sign).map(Array(x1, _)).toArray
        } else if (y1 == y2) {
            return x1.to(x2).by((x2 - x1).sign).map(Array(_, y1)).toArray
        } else {
            val a = (y2 - y1)/(x2 - x1)
            val b = y1 - a * x1
            return x1.to(x2).by((x2 - x1).sign).map(x => Array(x, a*x + b)).toArray
        }
    }
    val vents = Source.fromFile("input.txt").getLines().
        map(_.split(" -> ").map(_.trim.split(",").map(_.toInt))).toList

    val part_one_points = vents.
        withFilter(p => p(0)(0) == p(1)(0) || p(0)(1) == p(1)(1)).
        flatMap(p => get_points_between(p(0), p(1))).map(_.toList).
        groupBy(identity).view.mapValues(_.size).toMap.
        filter(_._2 > 1).size
    println(s"Part One: ${part_one_points}")

    val part_two_points = vents.
        flatMap(p => get_points_between(p(0), p(1))).map(_.toList).
        groupBy(identity).view.mapValues(_.size).toMap.
        filter(_._2 > 1).size
    println(s"Part Two: ${part_two_points}")
}
