package homeworks.collections


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
   * @param offset сдвиг вперёд по алфавиту, неотрицательное целое число.
   * @return зашифрованное слово
   */
  def encrypt(word: String, offset: Int): String = {
    val newOffset: Int = offset % Alphabet.length
    word.foldLeft("") {
      case (acc: String, char: Char) =>
        val idx: Int = Alphabet.indexOf(char)
        val shift: Int = idx + newOffset
        if (idx > -1) {

          val newChar: Char =
            if (shift > (Alphabet.length - 1)) {
              val ni: Int = shift - Alphabet.length
              Alphabet(ni)
            } else {
              Alphabet(shift)
            }

          acc :+ newChar

        } else {
          throw new CipherException(s"unsupported alphabet[$char]")
        }
    }
  }

  /**
   * @param cipher шифр, который необходимо расшифровать
   * @param offset сдвиг вперёд по алфавиту, неотрицательное целое число.
   * @return расшифрованное слово
   */
  def decrypt(cipher: String, offset: Int): String = {
    val newOffset: Int = offset % Alphabet.length
    cipher.foldLeft("") {
      case (acc: String, char: Char) =>
        val idx: Int = Alphabet.indexOf(char)
        val shift: Int = idx - newOffset
        if (idx > -1) {

          val newChar: Char =
            if (shift < 0) {
              val ni: Int = Alphabet.length + shift
              Alphabet(ni)
            } else {
              Alphabet(shift)
            }

          acc :+ newChar

        } else {
          throw new CipherException(s"unsupported alphabet[$char]")
        }
    }
  }

}
