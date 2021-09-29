package adt

object Adt {

  final case class Player(name: String, bet: Bet)

  final case class GameResult(generatedNumber: LimitedNumber, player: Player) {
    import Bet._
    val gain = player.bet match {
      case strait: Strait
        if strait.number == generatedNumber.number => 35 * strait.cash
      case splitUp: SplitUp
        if check(splitUp.firstNumber, splitUp.lastNumber, generatedNumber.number) => 17 * splitUp.cash
      case streetBet: StreetBet
        if check(streetBet.firstNumber, streetBet.lastNumber, generatedNumber.number) => 11 * streetBet.cash
      case cornerBet: CornerBet
        if check(cornerBet.firstNumber, cornerBet.lastNumber, generatedNumber.number) => 8 * cornerBet.cash
      case sixLineBet: SixLineBet
        if check(sixLineBet.firstNumber, sixLineBet.lastNumber, generatedNumber.number) => 5 * sixLineBet.cash
      case odd: Odd if generatedNumber.number % 2 == 1 => 2 * odd.cash
      case even: Even if generatedNumber.number % 2 == 0 => 2 * even.cash
      case _ => 0
    }

    override def toString: String = player.name + " " + gain.toString
    private  def check(a: Int, b: Int, x: Int): Boolean = {
      if(a <= x &&  x <= b) true else false
    }
  }

  sealed trait Bet
  object Bet {
    abstract case class Strait(number: Int, cash: Int) extends Bet
    abstract  case class SplitUp(firstNumber: Int, lastNumber: Int, cash: Int) extends Bet
    abstract case class StreetBet(firstNumber: Int, lastNumber: Int, cash: Int) extends Bet
    abstract case class CornerBet(firstNumber: Int, lastNumber: Int, cash: Int) extends Bet
    abstract case class SixLineBet(firstNumber: Int, lastNumber: Int, cash: Int) extends Bet
    abstract case class DozenBet(firstNumber: Int, lastNumber: Int, cash: Int) extends Bet
    final case class Even(cash: Int) extends Bet
    final case class Odd(cash: Int) extends Bet

    sealed trait IntervalBet {
      def create(firstNumber: Int, lastNumber: Int, cash: Int): Either[String, SplitUp] =
        (firstNumber, lastNumber) match {
          case (f, s) if f > 36 || f < 1 || s > 36 || s < 1 => Left("Wrong numbers")
          case _ => Right(new SplitUp(firstNumber, lastNumber, cash){})
        }
    }
    case object Strait {
      def create(number: Int, cash: Int): Either[String, Strait] = number match {
        case number if number > 36 || number < 1 => Left("Wrong number")
        case _ => Right(new Strait(number, cash){})
      }
    }
    case object SplitUp extends IntervalBet
    case object StreetBet extends IntervalBet
    case object CornerBet extends IntervalBet
    case object SixLineBet extends IntervalBet
    case object DozenBet extends IntervalBet
  }


  sealed abstract case class LimitedNumber (number: Int)
  object LimitedNumber{
    def create(number: Int): Either[String, LimitedNumber] = number match{
      case number if  number > 36 || number < 1 => Left("Wrong number")
      case _ => Right(new LimitedNumber(number){})
    }
  }

  def generateNumber(): Either[String, LimitedNumber] = {
    val random = scala.util.Random
    LimitedNumber.create(random.nextInt(36) + 1)
  }

  def runGame(generatedNumber: LimitedNumber, players: List[Player]): List[GameResult] = {
    players.map(x => GameResult(generatedNumber, x))
  }


  def main(args: Array[String]): Unit = {
    import Bet._
    val players = List(Player("Ivan", Strait.create(15, 130).toOption.get),
                 Player("Dima", Strait.create(2, 25).toOption.get),
                 Player("Kim", SplitUp.create(5,6, 100).toOption.get),
      Player("Din", Even(10)),
      Player("Vasua", Odd(15)),
      Player("Eugeni", CornerBet.create(5,8, 100).toOption.get),
      Player("Denis", DozenBet.create(13,24, 100).toOption.get)
    )

   print( runGame( generateNumber().toOption.get, players))
  }
}
