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
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def evalChar(char: Char): Int = {
      if (char == '(') 1
      else if (char == ')') -1
      else 0
    }

    def balanceRec(chars: List[Char], acc: Int): Boolean = {
      if (acc == 0 && chars.isEmpty) true
      else if (acc < 0) false
      else if (acc > 0 && chars.isEmpty) false
      else balanceRec(chars.tail, evalChar(chars.head) + acc)
    }

    if (chars.isEmpty) true
    else balanceRec(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
