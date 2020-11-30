package com.softwaremill.work

import scala.deriving.Mirror
import scala.compiletime.constValue
import scala.compiletime.erasedValue
import scala.compiletime.summonFrom
import scala.compiletime.summonInline

object ToMap extends App:
  case class Address(street: String, number: Long)
  case class Person(address: Address) //name: String, age: Int, 

  //

  inline def tupleTypeToString[A <: Tuple]: List[String] = inline erasedValue[A] match {
    case _: EmptyTuple => Nil
    case _: (head *: tail) =>
      val headStr = constValue[head].toString
      val tailStr = tupleTypeToString[tail]
      headStr :: tailStr
  }

  inline def labels[T](using m: Mirror.ProductOf[T]): List[String] =
    tupleTypeToString[m.MirroredElemLabels]

  //

  transparent inline def summonInlineOpt[T]: Option[T] = summonFrom {
    case t: T => Some(t)
    case _ => None
  }

  inline def fieldValues[L <: Tuple, V <: Tuple](values: List[Any]): Map[String, Any] = inline (erasedValue[L], erasedValue[V]) match {
    case (_: (headLabel *: tailLabels), _: (headType *: tailTypes)) =>

      val nested = summonFrom {
        case _: Mirror.Of[headType] => 1//
        case _ => 0 //value
      }

      val label = constValue[headLabel].toString
      val value = values.head
      val rest = fieldValues[tailLabels, tailTypes](values.tail)

      //      val n2 = inline summonInlineOpt[Mirror.ProductOf[headType]] match {
      //        case Some(x) => 
      //          mapOfMaps[headType](value.asInstanceOf[headType])(using x)
      //        case None => value
      //      }

      rest ++ Map(label -> nested)
    case _ => Map.empty
  }

  inline def mapOfMaps[T](t: T)(using m: Mirror.ProductOf[T]): Map[String, Any] =
    fieldValues[m.MirroredElemLabels, m.MirroredElemTypes](t.asInstanceOf[Product].productIterator.toList)

  println(labels[Person])
  println(labels[Address])
  //  println(mapOfMaps(Person("x", 10, Address("z", 10))))
  println(mapOfMaps(Address("z", 10)))
  println(mapOfMaps(Person(Address("z", 10))))