import com.data.MatrixFileCreator
import com.logic.MatrixCalculator
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.{convertToAnyShouldWrapper, equal}



class ExelCalculatorSpec extends AnyFunSpec{

  val inputMatrixOne: Array[Array[String]] = Array(Array("12", "=C2", "3"),
    Array("=A1+B1*C1/5", "=A2*B1", "=B3-C3"),Array("'Test", "=4-3", "5"))

  val inputMatrixTwo: Array[Array[String]] = Array(Array("=B2" , "2"), Array("'Red", "=A1"))

  val inputMatrixThree: Array[Array[String]] = Array(Array("1", "=C2", "2"),
    Array("=A1+B1*C1/5", "=A2*B1", "=B3-C3"),Array("'Test", "=4-3", "=B1"))

  val inputMatrixFour: Array[Array[String]] = Array(Array("12", "=C2", "3"),
    Array("=A1+B1*C1/5", "=A2*B1", "'Tea"),Array("'Test", "=4-3", "5"))

  val inputMatrixFive: Array[Array[String]] = Array(Array("12", "=C2", "3"),
    Array("=A1+B1*C1/0", "=A2*B1", "=B3-C3"),Array("'Test", "=4-3", "5"))

  val calculator = new MatrixCalculator(inputMatrixOne)

   describe("calculating matrix") {
     it("should calculate the matrix correctly") {
       calculator.calculate() should equal(Array(Array("12", "-4", "3"),
         Array("4", "-16", "-4"), Array("Test", "1", "5")))
     }
   }

  describe("cycle error"){
    it("should be #cycle in a cell if the cell is in cycle"){
      calculator.calculateCell(0,0, inputMatrixTwo) should equal("#cycle")
      calculator.calculateCell(1,0, inputMatrixThree) should equal("#cycle")
    }
  }

  describe("not all numbers error"){
    it("should be #not number error if attempting to do operations with text lines"){
      calculator.calculateCell(0,1, inputMatrixFour) should equal("#not all numbers")
      calculator.calculateCell(1,0, inputMatrixFour) should equal("#not all numbers")
    }
  }

  describe("division by zero"){
    it("should be #/ by zero error if attempting division by zero"){
      calculator.calculateCell(1,0, inputMatrixFive) should equal("#/ by zero")
      calculator.calculateCell(1,1, inputMatrixFive) should equal("#/ by zero")
    }
  }

}
