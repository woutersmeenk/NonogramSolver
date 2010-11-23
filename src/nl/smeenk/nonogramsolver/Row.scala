package nl.smeenk.nonogramsolver

trait Box
case class White() extends Box
case class Black() extends Box
case class Unknown() extends Box

case class Row(
  val boxes: List[Box]) {

  override def toString: String = {
    boxes.map { box: Box =>
      box match {
        case Black() => '#'
        case White() => ' '
        case Unknown() => '-'
      }
    }.mkString("");
  }

  /**
   * Returns true if the row fits the clues else false
   */
  def check(clues: List[Int]): Boolean = {
    clues match {
      case clue :: restClues =>
        boxes match {
          case Black() :: _ =>
            val front = boxes.take(clue)
            val back = boxes.drop(clue)

            front.length == clue &&
              front.forall { _ == Black() } &&
              (back match {
                case White() :: _ => new Row(back).check(restClues)
                case _ :: _ => false
                case Nil => restClues.isEmpty
              })
          case White() :: rest =>
            // Cast should not be nessery
            new Row(rest.asInstanceOf[List[Box]]).check(clues)
          case _ => false
        }
      case Nil => boxes.forall { _ == White() }
    }
  }

  /**
   * Returns the row with black and white filled in where
   *  there are only blacks or whites in that place in all rows
   */
  def solve(rows: List[Row], clues: List[Int]): Row = {
    val start = List.fill(boxes.size)(0) : List[Int]
    val counts = rows.foldRight(start) { (row: Row, buf: List[Int]) =>
      row.boxes.zip(buf).map { tuple: (Box, Int) =>
        val (box, count) = tuple
        box match {
          case Black() => count + 1
          case _ => count
        }
      }
    }
    val newBoxes = counts.map { count: Int =>
      if (count == 0) {
        White()
      } else if (count == rows.length) {
        Black()
      } else {
        Unknown()
      }
    }
    new Row(newBoxes)
  }

  /**
   * Returns a list of all the possible filled in rows
   */
  def findSolutions(clues: List[Int]): List[Row] = {
    Row.findSolutions(boxes, clues).map { newBoxes: List[Box] =>
      new Row(newBoxes)
    }
  }
}

object Row {
  /**
   * Returns a list of all the possible filled in rows
   */
  private def findSolutions(row: List[Box], clues: List[Int]): List[List[Box]] = {
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

    def white(row: List[Box], clues: List[Int]) = findSolutions(row, clues).map { White() :: _ }

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
  private def placeClue(row: List[Box], clue: Int): Option[List[Box]] = {
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