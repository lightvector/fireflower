/**
  * CardPropertyMap.scala
  * A basic mutable structure for storing sets of properties known about CardIds.
  */

package fireflower
import scala.reflect.ClassTag
import scala.collection.mutable.Queue

object CardPropertyMap {
  def apply[T](rules: Rules): CardPropertyMap[T] = {
    new CardPropertyMap[T](Array.fill(rules.deckSize)(Queue[T]()))
  }
}

class CardPropertyMap[T] private (
  val arr: Array[Queue[T]]
) {

  def apply(cid: CardId): Queue[T] = {
    arr(cid)
  }

  def add(cid: CardId, value: T): Unit = {
    arr(cid).enqueue(value)
  }

  def filterOut(cid: CardId)(f: T => Boolean): Unit = {
    val _ = arr(cid).dequeueAll(f)
  }

  def remove(cid: CardId, value: T): Unit = {
    filterOut(cid) { x => x == value }
  }
}
