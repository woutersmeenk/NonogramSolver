package nl.smeenk.nonogramsolver

object NonogramSolver {
  def main(args: Array[String]) {
    val row = Unknown() :: Unknown() :: Unknown() :: Unknown() :: Unknown() :: Nil
    val clues = 1 :: 2 :: Nil
    val sols = findSolutions(row, clues)
    sols.foreach(println)
  }

  type Row = List[Box]
  type Clues = List[Int]

  /**
   * Returns true if the row fits the clues else false
   */
  def check(row: Row, clues: Clues): Boolean = {
    clues match {
      case clue :: restClues =>
        row match {
          case Black() :: _ =>
            val front = row.take(clue)
            val back = row.drop(clue)

            front.length == clue &&
              front.forall { _ == Black() } &&
              (back match {
                case White() :: _ => check(back, restClues)
                case _ :: _ => false
                case Nil => restClues.isEmpty
              })
          case White() :: rest =>
            check(rest, clues)
          case _ => false
        }
      case Nil => row.forall { _ == White() }
    }
  }

  /**
   * Returns a list of all the possible filled in rows
   */
  def findSolutions(row: Row, clues: Clues): List[Row] = {
    def black(clue: Int, restClues: List[Int]) = {
      placeClue(row, clue) match {
        case None => Nil
        case Some(newRow) =>
          if (restClues.isEmpty) {
            findSolutions(newRow.drop(clue), restClues).map { newRow.take(clue) ::: _ }
          } else {
            findSolutions(newRow.drop(clue + 1), restClues).map { newRow.take(clue + 1) ::: _ }
          }
      }
    }

    def white(row: Row, clues: List[Int]) = findSolutions(row, clues).map { White() :: _ }

    clues match {
      case clue :: restClues =>
        row match {
          case Black() :: _ => black(clue, restClues)
          case White() :: rest => white(rest, clues)
          case Unknown() :: rest => black(clue, restClues) ::: white(rest, clues)
          case Nil => Nil
        }
      case Nil =>
        if (row.contains(Black())) {
          Nil
        } else {
          row.map { _ => White() } :: Nil
        }
    }
  }

  /**
   * Returns a row with the clue placed if the placement is valid
   */
  def placeClue(row: Row, clue: Int): Option[Row] = {
    if (clue == 0) {
      row match {
        case Black() :: _ => None
        case _ :: rest => Some(White() :: rest)
        case Nil => Some(row)
      }
    } else {
      row match {
        case White() :: _ => None
        case _ :: rest =>
          placeClue(rest, clue - 1) match {
            case None => None
            case Some(row) => Some(Black() :: row)
          }
        case Nil => None
      }
    }
  }
}

trait Box
case class White() extends Box
case class Black() extends Box
case class Unknown() extends Box
