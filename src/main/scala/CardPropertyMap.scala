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
}

class CardPropertyMap[T] private (
  val arr: Array[List[T]]
) {

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

  def remove(cid: CardId, value: T): Unit = {
    filterOut(cid) { x => x == value }
  }
}
