/**
  * CardPropertyMap.scala
  * A basic mutable structure for storing sets of properties known about CardIds.
  */

package fireflower
import scala.reflect.ClassTag

object CardPropertyMap {
  def apply[T](rules: Rules): CardPropertyMap[T] = {
    new CardPropertyMap[T](Array.fill(rules.deckSize)(List[T]()))
  }
  def apply[T](that: CardPropertyMap[T]) = {
    new CardPropertyMap[T](that.arr.clone())
  }
}

class CardPropertyMap[T] private (
  val arr: Array[List[T]]
) {

  def copyTo(other: CardPropertyMap[T]): Unit = {
    Array.copy(arr, 0, other.arr, 0, arr.size)
  }

  //The most recently added values are at the front of the list
  def apply(cid: CardId): List[T] = {
    arr(cid)
  }

  def add(cid: CardId, value: T): Unit = {
    arr(cid) = value :: arr(cid)
  }

  def filterOut(cid: CardId)(f: T => Boolean): Unit = {
    arr(cid) = arr(cid).filterNot(f)
  }

  def pop(cid: CardId): Unit = {
    if(arr(cid).nonEmpty) {
      arr(cid) = arr(cid).tail
    }
  }

  def remove(cid: CardId, value: T): Unit = {
    filterOut(cid) { x => x == value }
  }
}
