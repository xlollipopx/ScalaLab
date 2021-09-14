//package adt
//import adt.Adt.Bet.{CornerBet, DozenBet, Even, Odd, SplitUp, Strait}
//import adt.Adt.{LimitedNumber, Player, generateNumber, runGame}
//import org.scalatest.FunSuite
//
//object AdtTest {
//
//
//  class NumberGeneratorTest extends FunSuite {
//    test("generateNumber should return value in 1 to 36 interval") {
//      assert(generateNumber().toOption.get.number < 37 && generateNumber().toOption.get.number > 0)
//    }
//  }
//
//  class RunGameTest extends FunSuite {
//    val players = List(Player("Ivan", Strait.create(15, 130).toOption.get),
//      Player("Dima", Strait.create(2, 25).toOption.get),
//      Player("Kim", SplitUp.create(5,6, 100).toOption.get),
//      Player("Din", Even(10)),
//      Player("Vasua", Odd(15)),
//      Player("Eugeni", CornerBet.create(5,8, 100).toOption.get),
//      Player("Denis", DozenBet.create(13,24, 100).toOption.get)
//    )
//
//    val playersExpected = List("Ivan 0", "Dima 0", "Kim 0", "Din 0", "Vasua 30", "Kim 0", "Kim 1700")
//
//    test("runGame") {
//      assert(runGame( LimitedNumber.create(5).toOption.get, players).
//        map(x => x.toString) == playersExpected)
//    }
//  }
//
//}
