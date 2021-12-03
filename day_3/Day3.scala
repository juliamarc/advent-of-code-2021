import scala.io.Source

object Day3 extends App {
    def generator_condition = (x:Int, l:Int) => if (x >= l/2) 1 else 0
    def scrubber_condition = (x:Int, l:Int) => if (x < l/2) 1 else 0
    def get_rating(r: List[List[Int]], f:(Int, Int) => Int) : Int = {
        var filtered_report = r
        var i = 0
        while (filtered_report.length > 1) {
            val mask = filtered_report.
                reduce(((x, y) => x.lazyZip(y).map(_ + _))).
                map(x => f(x, filtered_report.length))
            filtered_report = filtered_report.filter(x => x(i) == mask(i))

            i += 1
        }

        return Integer.parseInt(
            filtered_report(0).map(x => x.toString).mkString(""), 2)
    }

    val report = Source.fromFile("input.txt").getLines().map(x => x.toList.map(d => d.asDigit)).toList

    val gamma_rate = Integer.parseInt(
        report.
            reduce(((x, y) => x.lazyZip(y).map(_ + _))).
            map(x => if (x > report.length/2) '1' else '0').
            mkString(""),
        2)
    val epsilon_rate = Integer.parseInt(
        gamma_rate.toBinaryString.
            map(x => if (x == '1') '0' else '1').
            mkString(""),
        2)
    println(s"Part One: ${gamma_rate * epsilon_rate}")

    val generator_rating = get_rating(report, generator_condition)
    val scrubber_rating = get_rating(report, scrubber_condition)
    println(s"Part Two: ${generator_rating * scrubber_rating}")
}
