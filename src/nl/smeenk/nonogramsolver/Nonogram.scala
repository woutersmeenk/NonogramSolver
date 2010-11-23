package nl.smeenk.nonogramsolver

import java.io.File
import scala.io.Source

object Nonogram {

  def fromFile(file: File) = {
    val lines = Source.fromFile(file).getLines.filterNot(_.isEmpty).toList
    val sizes = lines(0)
    val spaceIndex = sizes.indexOf(" ");
    val (width, height) = sizes.trim.splitAt(spaceIndex)
    val widthInt = Integer.parseInt(width.trim)
    val heightInt = Integer.parseInt(height.trim)
    val rowClues = List.range(0, widthInt).map { index: Int =>
      val line = lines(index + 1).trim
      if (line.isEmpty) {
        Nil
      } else {
        line.split(" ").map { clue: String =>
          Integer.parseInt(clue.trim)
        }.toList
      }
    }
    val columnClues = List.range(0, heightInt).map { index: Int =>
      val line = lines(index + 1 + widthInt).trim
      if (line.isEmpty) {
        Nil
      } else {
        line.split(" ").map { clue: String =>
          Integer.parseInt(clue.trim)
        }.toList
      }
    }
    val rows = List.range(0, heightInt).map { index: Int =>
      new Row(List.fill(widthInt)(Unknown()))
    }
    new Nonogram(rows, rowClues, columnClues)
  }
}

class Nonogram(
  val rows: List[Row],
  val rowClues: List[List[Int]],
  val columnClues: List[List[Int]]) {

  def solveRows(): Nonogram = {
    val newRows = rows.zip(rowClues).map { tuple: (Row, List[Int]) =>
      val (row, clues) = tuple
      val rows = row.findSolutions(clues)
      row.solve(rows, clues)
    }
    new Nonogram(newRows, rowClues, columnClues)
  }

  def rotate(): Nonogram = {
    val newRows = List.range(0, columnClues.size).map { index: Int =>
      val newBoxes = rows.map { row: Row =>
        row.boxes(index)
      }
      new Row(newBoxes)
    }
    new Nonogram(newRows, columnClues, rowClues)
  }

  def solve(count: Int): Nonogram = {
    val newNonogram = solveRows.rotate.solveRows.rotate
    println(count)
    newNonogram.rows.foreach(println)

    if (rows == newNonogram.rows) {
      newNonogram
    } else {
      newNonogram.solve(count + 1)
    }
  }
}