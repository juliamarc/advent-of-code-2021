import scala.io.Source

object Day11 extends App {
    def step(energy_levels: Array[Array[Int]]) : Int = {
        var energy_levels_after = energy_levels
        for (coords <- energy_levels_coords) {
                val (x, y) = (coords(0), coords(1))
                energy_levels_after(x)(y) += 1
        }

        var total_flashes = 0
        var to_flash = 0
        do { 
            for (coords <- energy_levels_coords) {
                val (x, y) = (coords(0), coords(1))
    
                if (energy_levels_after(x)(y) > 9) {
                    total_flashes += 1
                    energy_levels_after(x)(y) = 0

                    val neighbors_coords_to_check = Array(
                        Array(x - 1, y), Array(x + 1, y),
                        Array(x, y - 1), Array(x, y + 1),
                        Array(x - 1, y - 1), Array(x + 1, y + 1),
                        Array(x + 1, y - 1), Array(x - 1, y + 1))
                    for (neighbor <- neighbors_coords_to_check) {
                        val (x_n, y_n) = (neighbor(0), neighbor(1))

                        if (energy_levels_after(x_n)(y_n) > 0) {
                            energy_levels_after(x_n)(y_n) += 1
                        }
                    }
                }
            }
            to_flash = energy_levels_coords.filter(c => energy_levels_after(c(0))(c(1)) > 9).size
        } while (to_flash > 0)

        return total_flashes
    }

    val energy_levels = Source.fromFile("input.txt").getLines().map(_.split("").map(_.toInt).toArray).toArray
    val energy_levels_height = energy_levels.length
    val energy_levels_width = energy_levels(0).length
    val energy_levels_padded = Array.fill(energy_levels_width + 2)(-1) +:
        energy_levels.map(-1 +:_ :+ -1) :+
        Array.fill(energy_levels_width + 2)(-1)
    val energy_levels_coords = (1 to energy_levels_height).
        flatMap(
            x => (1 to energy_levels_width).
            zipAll(List.fill(energy_levels_width)(x), x, x)).
        map(x => Array(x._1, x._2))

    val energy_levels_live_part_one = energy_levels_padded.map(_.clone)
    val steps_flashes = (1 to 100).
        map(x => step(energy_levels_live_part_one)).
        reduce(_ + _)
    println(s"Part One: ${steps_flashes}")

    val energy_levels_live_part_two = energy_levels_padded.map(_.clone)
    val all_flashes_step = (1 to 1000).
        map(x => (x, step(energy_levels_live_part_two))).
        filter(_._2 == 100).
        map(_._1).
        head
    println(s"Part Two: ${all_flashes_step}")
}
