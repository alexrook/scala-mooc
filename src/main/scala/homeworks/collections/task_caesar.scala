package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

object task_caesar {


  val Alphabet: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  class CipherException(msg: String) extends Exception(msg)

  /**
   * В данном задании Вам предлагается реализовать функции,
   * реализующие кодирование/декодирование строки шифром Цезаря.
   * https://ru.wikipedia.org/wiki/Шифр_Цезаря
   * Алфавит - прописные латинские буквы от A до Z.
   * Сдвиг   - неотрицательное целое число.
   * Пример: при сдвиге 2 слово "SCALA" шифруется как "UECNC".
   */

  /**
   * @param word   входное слово, которое необходимо зашифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return зашифрованное слово
   */
  def encrypt(word: String, offset: Int): String = {
    val newOffset: Int = offset % Alphabet.length
    word.foldLeft("") {
      case (acc: String, char: Char) =>
        val idx: Int = Alphabet.indexOf(char)
        if (idx > -1) {
          val newChar: Char = if ((idx + newOffset) > (Alphabet.length - 1)) {
            val ni = (idx + newOffset) - Alphabet.length
            Alphabet(ni)
          } else {
            Alphabet(idx + newOffset)
          }
          acc :+ newChar
        } else {
          throw new CipherException(s"unsupported alphabet[$char]")
        }
    }
  }

  /**
   * @param cipher шифр, который необходимо расшифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return расшифрованное слово
   */
  def decrypt(cipher: String, offset: Int): String = {
    val newOffset: Int = offset % Alphabet.length
    cipher.foldLeft("") {
      case (acc: String, char: Char) =>
        val idx: Int = Alphabet.indexOf(char)
        if (idx > -1) {
          val newChar: Char = if ((idx - newOffset) < 0) {
            val ni = Alphabet.length + (idx - newOffset)
            Alphabet(ni)
          } else {
            Alphabet(idx - newOffset)
          }
          acc :+ newChar
        } else {
          throw new CipherException(s"unsupported alphabet[$char]")
        }
    }
  }

}
