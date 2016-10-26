/**
  * RichTypes.scala
  * Implements some convenient functions not present in the Scala standard library, with implicit conversions
  * so that it's as if they were implemented for the standard library classes.
  */

package fireflower

import scala.language.implicitConversions
import scala.reflect.ClassTag

class RicherSeq[T](val seq: Seq[T]) {
  def findMap[U](f: T => Option[U]): Option[U] = seq.iterator.map(f).find(_.nonEmpty).flatten
  def filterNotFirst(f: T => Boolean): Seq[T] = {
    var filtering = true
    seq.filterNot { x => if(filtering && f(x)) { filtering = false; true } else false }
  }
  def partitionMap[U,V](f: T => Either[U,V]): (List[U],List[V]) = {
    var us: List[U] = List()
    var vs: List[V] = List()
    seq.foreach { x => f(x) match { case Left(u) => us = u :: us  case Right(v) => vs = v :: vs } }
    (us.reverse, vs.reverse)
  }
}
class RicherList[T](val list: List[T]) {
  def findMap[U](f: T => Option[U]): Option[U] = list.iterator.map(f).find(_.nonEmpty).flatten
  def filterNotFirst(f: T => Boolean): List[T] = {
    var filtering = true
    list.filterNot { x => if(filtering && f(x)) { filtering = false; true } else false }
  }
  def partitionMap[U,V](f: T => Either[U,V]): (List[U],List[V]) = {
    var us: List[U] = List()
    var vs: List[V] = List()
    list.foreach { x => f(x) match { case Left(u) => us = u :: us  case Right(v) => vs = v :: vs } }
    (us.reverse, vs.reverse)
  }
}
class RicherArray[T](val arr: Array[T]) {
  def findMap[U](f: T => Option[U]): Option[U] = arr.iterator.map(f).find(_.nonEmpty).flatten
  def filterNotFirst(f: T => Boolean): Array[T] = {
    var filtering = true
    arr.filterNot { x => if(filtering && f(x)) { filtering = false; true } else false }
  }
  def partitionMap[U:ClassTag,V:ClassTag](f: T => Either[U,V]): (Array[U],Array[V]) = {
    var us: List[U] = List()
    var vs: List[V] = List()
    arr.foreach { x => f(x) match { case Left(u) => us = u :: us  case Right(v) => vs = v :: vs } }
    (us.reverse.toArray, vs.reverse.toArray)
  }
}

class RicherMap[T,U](val map: Map[T,U]) {
  def update(key:T)(f: Option[U] => U): Map[T,U] = map + (key -> (f(map.get(key))))
  def change(key:T)(f: Option[U] => Option[U]): Map[T,U] = {
    f(map.get(key)) match {
      case None => map - key
      case Some(v) => map + (key -> v)
    }
  }
}

object RichImplicits {
  implicit def richifyList[T](list: List[T]) = new RicherList(list)
  implicit def richifyArray[T](arr: Array[T]) = new RicherArray(arr)
  implicit def richifySeq[T](seq: Seq[T]) = new RicherSeq(seq)
  implicit def richifyMap[T,U](map: Map[T,U]) = new RicherMap(map)

  //Also just a useful function
  def assertUnreachable(): Nothing = {
    throw new Exception("Bug - reached  code that should be unreachable")
  }
}
