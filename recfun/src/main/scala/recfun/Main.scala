package recfun

object Main {
  def main(args: Array[String]) {
    
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    
    println(balance("(if (zero? x) max (/ 1 x))".toList))
    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    println(balance(":-)".toList))
    println(balance("())(".toList))
    
    println(countChange(4, List(1, 2, 3, 4)))
    
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (r == 0) 1
      else if (c == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceAcc(chars: List[Char], count: Int): Boolean = {
        if (chars.isEmpty) (count == 0)
        else if (chars.head == '(') balanceAcc(chars.tail, count + 1)
        else if (chars.head == ')') {
          if (count == 0) false
          else balanceAcc(chars.tail, count - 1)
        }
        else balanceAcc(chars.tail, count)
      }
      balanceAcc(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      // we have an amount of money and some coin types.
      // we need something recursive and so we need to break this up.
      // one way to break it up is to count all the solutions that
      // include the coins.head and add that to the count of all
      // solutions that don't include coins.head. that will reduce
      // the number of coins and the money so we should terminate.
      if (money < 0 || coins.isEmpty) 0
      else if (money == 0) 1
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
