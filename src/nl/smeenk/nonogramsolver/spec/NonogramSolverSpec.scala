package nl.smeenk.nonogramsolver.test

import nl.smeenk.nonogramsolver.{ NonogramSolver, White, Black, Unknown }
import nl.smeenk.nonogramsolver.NonogramSolver.Row
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Test.Params

object NonogramSolverSpec extends Properties("NonogramSolver") {
  def generateRow(cluesAndSpaces: List[Int], black: Boolean = false): Row = {
    cluesAndSpaces match {
      case clueOrSpace :: rest =>
        val box = if (black) Black() else White()
        List.fill(clueOrSpace)(box) ::: generateRow(rest, !black);
      case Nil => Nil
    }
  }

  def getOdds[A](list: List[A]): List[A] = list match {
    case a :: b :: rest => b :: getOdds(rest)
    case _ => Nil
  }

  val nums = Gen.oneOf(List.range(1, 100))
  val lists = Gen.listOf(nums)
  val boxes = Gen.oneOf(White(), Black(), Unknown())
  val rows = Gen.listOf(boxes)

  property("check") = forAll(lists) { list: List[Int] =>
    classify(list.size == 0, "empty") {
      classify(list.size > 10, "large", "small") {
        val row = generateRow(list)
        val clues = getOdds(list)
        NonogramSolver.check(row, clues)
      }
    }
  }

  property("findSolutions") = forAll(rows, lists) { (row: Row, clues: List[Int]) =>
    clues.forall { _ > 0 } ==> {
      val rows = NonogramSolver.findSolutions(row, clues)
      collect(rows.size) {
        rows.forall { outRow: Row =>
          NonogramSolver.check(outRow, clues) &&
            row.size == outRow.size
        }
      }
    }
  }

  override def main(args: Array[String]) {
    check(new Params(minSuccessfulTests = 1000))
  }
}