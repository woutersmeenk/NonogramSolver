package nl.smeenk.nonogramsolver.spec

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
    classify(list.size == 0, "empty") {
      classify(list.size > 10, "large", "small") {
        val row = generateRow(list)
        val clues = getOdds(list)
        NonogramSolver.check(row, clues)
      }
    }
  }

  property("findSolutions") = forAll(rows, lists) { (row: Row, clues: List[Int]) =>
    val rows = NonogramSolver.findSolutions(row, clues)
    rows.size > 0 ==> {
      collect(rows.size) {
        rows.forall { outRow: Row =>
          NonogramSolver.check(outRow, clues) &&
            row.size == outRow.size
        }
      }
    }
  }

  property("findSolutions2") = forAll(nums, lists) { (spaces: Int, clues: List[Int]) =>
    val size = clues.sum + spaces + clues.size - 1

    val row = List.fill(size)(Unknown())
    val rows = NonogramSolver.findSolutions(row, clues)
    collect(rows.size) {
      rows.forall { outRow: Row =>
        NonogramSolver.check(outRow, clues) &&
          row.size == outRow.size
      }
    }
  }

  override def main(args: Array[String]) {
    //NonogramSolver.findSolutions(List.fill(10)(Unknown()), List(1, 2, 2)).foreach(println)
    check(new Params(minSuccessfulTests = 5000))
  }
}