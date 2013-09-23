package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  def pascal(c: Int, r: Int) : Int = {
    if (c == 0 || c == r) 1
    else if (c > 0 && c < r)
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    else throw new RuntimeException("" + c + " not in 0.." + r)
  }

  def balance(chars: List[Char]): Boolean = {
    def balanceWithLevel(chars: List[Char], level: Int) : Boolean = {
      if (chars.isEmpty)
        level == 0
      else if (chars.head == '(')
        balanceWithLevel(chars.tail, level + 1)
      else if (chars.head == ')')
        level > 0 && balanceWithLevel(chars.tail, level - 1)
      else
        balanceWithLevel(chars.tail, level)
    }
    balanceWithLevel(chars, 0)
  }

  def countChange(money: Int, coins: List[Int]) : Int = {
    def count(change: List[Int]) : Int = {
      if (change.sum == money) 1
      else if (change.sum > money) 0
      else coins.map(c =>
        if (change.isEmpty || c >= change.head)
          count(c :: change)
        else 0
      ).sum
    }
    count(List())
  }
}
