package http.routes.meta

import http.Game.{GuessGame, StartInput}

import java.util.concurrent.atomic.AtomicReference
import scala.util.Random

object Setup {

  def setUpValues(guessGame: AtomicReference[GuessGame], startInput: StartInput) = {
    val l    = startInput.min
    val r    = startInput.max
    val rand = l + Random.nextInt(r - l)
    guessGame.set(GuessGame(rand, startInput.attempts, l, r, isStarted = true))
    println(rand)
  }

}
