import scala.io.Source

object Day2 extends App {
    val course = Source.fromFile("input.txt").getLines().map(c => c.split(" +")).toList

    val coords = course.
        map{
            case (Array("forward", units)) => (units.toInt, 0)
            case (Array("up", units)) => (0, -units.toInt)
            case (Array("down", units)) => (0, units.toInt)
            case _ => (0, 0)}.
        reduce((x, y) => (x._1 + y._1, x._2 + y._2))
    println(s"Part One: ${coords._1 * coords._2}")

    val coords2 = course.
        map{
            case (Array("forward", units)) => (units.toInt, 0, 0)
            case (Array("up", units)) => (0, 0, -units.toInt)
            case (Array("down", units)) => (0, 0, units.toInt)
            case _ => (0, 0, 0)}.
        foldLeft((0, 0, 0))(
            (acc, tup) => (
                acc._1 + tup._1,
                acc._2 + acc._3 * tup._1,
                acc._3 + tup._3))
    println(s"Part Two: ${coords2._1 * coords2._2}")
}
