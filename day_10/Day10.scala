import scala.collection.mutable.Stack
import scala.io.Source

object Day10 extends App {
    val navigation_subsystem_code = Source.fromFile("input.txt").getLines()
    val left_parentheses = List('(', '[', '{', '<')
    val right_parentheses = List(')', ']', '}', '>')
    val left_right_map = left_parentheses.zip(right_parentheses).toMap
    val syntax_error_score_map = Map(
        Option(')') -> 3,
        Option(']') -> 57,
        Option('}') -> 1197,
        Option('>') -> 25137,
        None -> 0)
    val autocomplete_score_map = Map(
        Option(')') -> 1,
        Option(']') -> 2,
        Option('}') -> 3,
        Option('>') -> 4,
        None -> 0)

    var corruption_errors = Array[Array[Char]]()
    var incomplete_fixes = Array[Array[Char]]()
    for (line <- navigation_subsystem_code) {
        var errors = Array[Char]()
        val stack = Stack[Char]()
        for (character: Char <- line) {
            if (left_parentheses.contains(character)) {
                stack.push(character)
            }
            else if (right_parentheses.contains(character)) {
                val opening = stack.pop()
                val closing = character
                if (closing != left_right_map(opening)) errors :+= closing
            }
        }
        if (errors.length > 0) {
            corruption_errors :+= errors
        }
        else {
            incomplete_fixes :+= stack.map(left_right_map(_)).toArray
        }
    }

    val syntax_error_score = corruption_errors.
        map(e => e.headOption).
        map(syntax_error_score_map(_)).
        reduce(_ + _)
    println(s"Part One: ${syntax_error_score}")

    val autocomplete_score = incomplete_fixes.
        map(f => f.map(
            c => autocomplete_score_map(Option(c))).
            foldLeft(BigInt(0))((a, t) => a * 5 + t)).
        sortWith(_ > _).
        drop(incomplete_fixes.length/2).
        head
    println(s"Part Two: ${autocomplete_score}")
}
