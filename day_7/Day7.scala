import scala.io.Source

object Day7 extends App {
    val crabs = Source.fromFile("input.txt").getLines().next().split(",").map(_.toInt).toList

    val median = crabs.sortWith(_ < _).drop(crabs.length/2).head
    val fuel_part_one = crabs.map(p => (p - median).abs).reduce(_ + _)
    println(s"Part One: ${fuel_part_one}")

    val mean = crabs.reduce(_ + _) / crabs.length
    val fuel_part_two = crabs.map(p => 1.to((p - mean).abs).foldLeft(0)(_ + _)).reduce(_ + _)
    println(s"Part Two: ${fuel_part_two}")
}
