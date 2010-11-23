package nl.smeenk.nonogramsolver.spec

import nl.smeenk.nonogramsolver.{ NonogramSolver, Row, Box, White, Black, Unknown }
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Test.Params

object NonogramSolverSpec extends Properties("NonogramSolver") {
  def generateRow(cluesAndSpaces: List[Int], black: Boolean = false): List[Box] = {
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

  val nums = Gen.oneOf(List.range(1, 10))
  val lists = Gen.oneOf(Gen.listOfN(1, nums),
    Gen.listOfN(2, nums),
    Gen.listOfN(3, nums),
    Gen.listOfN(4, nums),
    Gen.listOfN(5, nums))
  val largeList = Gen.listOf(nums)
  val boxes = Gen.oneOf(White(), Black(), Unknown())
  val rows = Gen.listOf(boxes)

  property("check") = forAll(largeList) { list: List[Int] =>
    collect(list.size) {
      val row = new Row(generateRow(list))
      val clues = getOdds(list)
      row.check(clues)
    }
  }

  property("findSolutions mixed") = forAll(rows, lists) { (boxes: List[Box], clues: List[Int]) =>
    val row = new Row(boxes)
    val rows = row.findSolutions(clues)
    rows.size > 0 ==> {
      collect(rows.size) {
        rows.forall { outRow: Row =>
          outRow.check(clues) &&
            row.boxes.size == outRow.boxes.size
        }
      }
    }
  }

  property("findSolutions black or white") = forAll(largeList) { list: List[Int] =>
    collect(list.size) {
      val row = new Row(generateRow(list))
      val clues = getOdds(list)
      val rows = row.findSolutions(clues)
      rows.size == 1 && rows.head == row
    }
  }

  property("findSolutions unknowns") = forAll(nums, lists) { (spaces: Int, clues: List[Int]) =>
    val size = clues.sum + spaces + clues.size - 1

    val row = new Row(List.fill(size)(Unknown()))
    val rows = row.findSolutions(clues)
    collect(rows.size) {
      rows.forall { outRow: Row =>
        outRow.check(clues) &&
          row.boxes.size == outRow.boxes.size
      }
    }
  }

  override def main(args: Array[String]) {
    //NonogramSolver.findSolutions(List.fill(10)(Unknown()), List(1, 2, 2)).foreach(println)
    check(new Params(minSuccessfulTests = 5000))
  }
}