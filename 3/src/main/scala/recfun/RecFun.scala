package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 && r == 0) then 1
    else if (c < 0 || c > r) then 0
    else pascal(c - 1, r - 1) + pascal(c, r - 1)


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    innerBalance(chars, 0)


  def innerBalance(chars: List[Char], height: Int): Boolean =
    val value = if (chars.head == '(') then 1
                else if (chars.head == ')') then -1
                else 0
    val newHeight = height + value

    if (newHeight < 0) then false
    else if chars.length == 1 then true
    else innerBalance(chars.tail, newHeight)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) then 1
    else if (money < 0 || coins.isEmpty) then 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)