package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

object task_seq_riddle {

  /**
   * Рассмотрим последовательность с числами:
   *
   * 1  <- одно число
   * 1 1  <- одна единица, смотрим предыдущую последовательность
   * 2 1  <- две единицы
   * 1 2 1 1  <- одна двойка, одна единица
   * 1 1 1 2 2 1 <- одна единица, одна двойка, две единицы
   * 3 1 2 2 1 1 <- три единицы, две двойки, одна единица
   * ...........
   *
   * 1. Реализуйте функцию генерирующую след последовательность из текущей
   * */

  def nextLineV1(currentLine: List[Int]): List[Int] =
    currentLine.foldLeft(List.empty[(Int, Int)]) {
      case ((count, digit) :: tail, currDigit) =>
        if (currDigit == digit) {
          (count + 1, digit) +: tail
        } else {
          (1, currDigit) +: (count, digit) +: tail
        }
      case (Nil, currDigit) =>
        List(1 -> currDigit)
    }
      .reverse
      .foldLeft(List.empty[Int]) {
        case (ret: List[Int], (count, digit)) =>
          ret :+ count :+ digit
      }

  def nextLine(currentLine: List[Int]): List[Int] =
    currentLine.foldLeft(List.empty[(Int, Int)]) {
      case (acc: List[(Int, Int)], currDigit: Int) =>
        acc.lastOption match {
          case Some((digit, count)) =>
            if (digit == currDigit) {
              acc.init :+ (digit, count + 1)
            } else {
              acc :+ (currDigit, 1)
            }

          case None => List(currDigit -> 1)
        }

    }.foldLeft(List.empty[Int]) {
      case (ret: List[Int], (digit, count)) => ret :+ count :+ digit
    }

  /**
   * 2. Реализуйте ленивый список, который генерирует данную последовательность
   * Hint: см. LazyList.cons
   *
   * lazy val funSeq: LazyList[List[Int]]  ...
   *
   */

   val funSeq: LazyList[List[Int]] = LazyList.iterate(List(1))(nextLine)

}