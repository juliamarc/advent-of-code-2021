import scala.io.Source

object Day1 extends App {
    val measurements = Source.fromFile("input.txt").getLines().toList.map(m => m.toInt)

    val increases = measurements.
        sliding(2, 1).
        map(x => (x(0) - x(1)) < 0).
        count(x => x == true)
    println(s"Part One: $increases")

    val increases2 = measurements.
        sliding(3, 1).
        map(x => x.foldLeft(0)(_ + _)).
        sliding(2, 1).
        map(x => (x(0) - x(1)) < 0).
        count(x => x == true)
    println(s"Part Two: $increases2")
}
