import scala.io.Source

object Day5 extends App {
    def get_points_between(p1: Array[Int], p2:Array[Int]) : Array[Array[Int]] = {
        val (x1, y1, x2, y2) = (p1(0), p1(1), p2(0), p2(1))

        if (x1 == x2) {
            var y_range = 0 to 0
            if(y1 <= y2) {
                y_range = (y1 to y2)
            }  else {
                y_range = (y2 to y1)
            }
            return y_range.map(y => Array(x1, y)).toArray
        } else if (y1 == y2) {
            var x_range = 0 to 0
            if(x1 <= x2) {
                x_range = (x1 to x2)
            }  else {
                x_range = (x2 to x1)
            }
            return x_range.map(x => Array(x, y1)).toArray
        } else {
            val a = (y2 - y1)/(x2 - x1)
            val b = y1 - a * x1
            var x_range = 0 to 0
            if(x1 <= x2) {
                x_range = (x1 to x2)
            }  else {
                x_range = (x2 to x1)
            }
            return x_range.map(x => Array(x, a*x + b)).toArray
        }
    }
    val vents = Source.fromFile("input.txt").getLines().
        map(
            l => l.split(" -> ").map(
                p => p.trim.split(",").map(_.toInt))).toList

    val part_one_points = vents.
        withFilter(p => p(0)(0) == p(1)(0) || p(0)(1) == p(1)(1)).
        flatMap(p => get_points_between(p(0), p(1))).map(_.toList).
        groupBy(identity).
        view.mapValues(_.size).toMap.
        filter(x => x._2 > 1).size
    println(s"Part One: ${part_one_points}")

    val part_two_points = vents.
            flatMap(p => get_points_between(p(0), p(1))).map(_.toList).
            groupBy(identity).
            view.mapValues(_.size).toMap.
            filter(x => x._2 > 1).size
    println(s"Part Two: ${part_two_points}")
}
