package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("Testing balance")
    val testStr = "())("
    println(testStr)
    println(balance(testStr.toList))

    println("Testing change")
    val money = 4
    val coins = List(1, 2)
    println(money)
    println(coins)
    println(countChange(money, coins))

  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0) {
        1
      }
      else if (c < 0 || c > r) {
        0
      }
      else pascal(c, r-1) + pascal(c-1, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def getCount(char: Char): Int = {
        if (char == '(') {
          1
        }
        else if (char == ')') {
          -1
        }
        else 0
      }

      def parenCount(chars: List[Char], count: Int): Boolean = {
        if(chars.isEmpty) {
          if (count == 0) {
            true
          }
          else false
        }
        else if (count < 0) {
          false
        }
        else parenCount(chars.tail, count + getCount(chars.head))
      }

      parenCount(chars, 0)

    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty) {
        if (money > 0) {
          0
        }
        else 1
      }
      else if (money < 0) {
        0
      }
      else countChange(money - coins.head, coins)  +
        countChange(money, coins.tail)
    }
  }
