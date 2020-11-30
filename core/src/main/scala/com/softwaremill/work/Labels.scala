package com.softwaremill.work

import scala.compiletime.{constValue, erasedValue, summonFrom, summonInline}
import scala.deriving.Mirror

// https://github.com/lampepfl/dotty/issues/10508
object Labels extends App:
  case class Address(street: String, number: Long)
  case class Person(name: String, age: Int, address: Address)

  //

  inline def allLabelsNested[L <: Tuple, V <: Tuple]: List[Any] = inline (erasedValue[L], erasedValue[V]) match {
    case (_: (headLabel *: tailLabels), _: (headType *: tailTypes)) =>

      val label = constValue[headLabel].toString
      val rest = allLabelsNested[tailLabels, tailTypes]

      val nested = summonFrom {
        case given _: Mirror.ProductOf[headType] => List(allLabels[headType])
        case _ => Nil
      }

      List(label) ++ nested ++ rest
    case _ => Nil
  }

  inline def allLabels[T](using m: Mirror.ProductOf[T]): List[Any] =
    allLabelsNested[m.MirroredElemLabels, m.MirroredElemTypes]

  println(allLabels[Address])
  println(allLabels[Person])
