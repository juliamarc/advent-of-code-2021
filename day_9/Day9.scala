import scala.io.Source

object Day9 extends App {
    def count_basin_neighbors(start_coords: Array[Int]) : Int = {
        val (x, y) = (start_coords(0), start_coords(1))
        if (heightmap_padded_visited(x)(y) >= 9) return 0
        heightmap_padded_visited(x)(y) = 10

        val neighbors_coords_to_check = Array(
            Array(x - 1, y),
            Array(x + 1, y),
            Array(x, y - 1),
            Array(x, y + 1)).
            filter(c => heightmap_padded_visited(c(0))(c(1)) < 9)
        val neighbors_sum = neighbors_coords_to_check.
            map(count_basin_neighbors(_)).
            foldLeft(0)(_ + _)

        return neighbors_sum + 1
    }

    val heightmap = Source.fromFile("input.txt").getLines().map(_.split("").map(_.toInt).toArray).toArray
    val heightmap_height = heightmap.length
    val heightmap_width = heightmap(0).length
    val heightmap_padded = Array.fill(heightmap_width + 1)(10) +:
        heightmap.map(10 +:_ :+ 10) :+
        Array.fill(heightmap_width + 1)(10)

    var highpoints_coords = Array[Array[Int]]()
    for (x <- 1 to heightmap_height) {
        for (y <- 1 to heightmap_width) {
            val height = heightmap_padded(x)(y)
            val adjacent = Array(
                heightmap_padded(x - 1)(y),
                heightmap_padded(x + 1)(y),
                heightmap_padded(x)(y - 1),
                heightmap_padded(x)(y + 1))
            if (adjacent.map(_ > height).forall(_ == true))
                highpoints_coords :+= Array(x, y)
        }
    }
    val risk_levels_sum = highpoints_coords.
        map(c => heightmap_padded(c(0))(c(1))).
        foldLeft(0)(_ + _ + 1)
    println(s"Part One: ${risk_levels_sum}")

    var heightmap_padded_visited = heightmap_padded
    val largest_basins_product = highpoints_coords.
        map(count_basin_neighbors(_)).
        sortWith(_ > _).
        take(3).
        reduce(_ * _)
    println(s"Part Two: ${largest_basins_product}")
}
