package homeworks.futures

import homeworks.HomeworksUtils.TaskSyntax

import scala.concurrent.{ExecutionContext, Future}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правом результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */

  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежем из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] =
    futures.map {
      f: Future[A] =>
        f.map {
          a: A => List(a) -> List.empty[Throwable]
        }.recover {
          case ex: Throwable => List.empty[A] -> List(ex)
        }
    }.reduce {
      (leftF: Future[(List[A], List[Throwable])], rightF: Future[(List[A], List[Throwable])]) =>
        for {
          (lListA, lListEx) <- leftF
          (rListA, rListEx) <- rightF
        } yield (lListA ++ rListA) -> (lListEx ++ rListEx)
    }

}
