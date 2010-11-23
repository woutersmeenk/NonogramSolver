package nl.smeenk.nonogramsolver

import java.io.File

object NonogramSolver {
  def main(args: Array[String]) {
    val nonogram = Nonogram.fromFile(new File("puzzle.txt"))
    var sol = nonogram.solve(1);
  }
}

