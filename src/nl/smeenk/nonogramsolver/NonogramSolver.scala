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
            val front = row take clue
            val back = row drop clue

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
    clues match {
      case clue :: restClues =>
        row match {
          case Black() :: rest => placeClue(row, clue).toList
          case White() :: rest => findSolutions(rest, clues).map { White() :: _ }
          case Unknown() :: rest =>
            placeClue(row, clue).toList ::: findSolutions(rest, restClues)
          case Nil => Nil
        }
      case Nil =>
        row match {
          case Black() :: rest => Nil
          case _ :: rest => findSolutions(rest, Nil).map { White() :: _ }
          case Nil => Nil
        }
    }
  }

  /**
   * Returns a row with the clue placed if the placement is valid
   */
  def placeClue(row: Row, clue: Int): Option[Row] =
    if (clue == 0) {
      row match {
        case Black() :: _ => None
        case _ :: rest => Some(White() :: row)
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

trait Box
case class White() extends Box
case class Black() extends Box
case class Unknown() extends Box
