package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r) 1 else pascal(c, r-1) + pascal(c-1, r-1)
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      val charCounter = (x:Char) => {
        if (x == '(') 1
        else if (x == ')') -1
        else 0
      }

      def helper:(List[Char], Int) => Boolean = (x: List[Char], y: Int) => {
        if (y < 0) false
        else {
          x match {
            case head :: tail => helper(tail, y + charCounter(head))
            case Nil => y == 0
          }
        }
      }

      helper(chars, 0)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def validCoins(moneyLeft: Int, coinsLeft: List[Int]): Int = {
        if (moneyLeft == 0) 1
        else if (moneyLeft < 0 || coinsLeft.isEmpty) 0
        else validCoins(moneyLeft, coinsLeft.tail) + validCoins(moneyLeft - coinsLeft.head, coinsLeft)
      }

      validCoins(money, coins)
    }
  }

