package com.softwaremill.funscala

import scala.Tuple._

object S170_Tuples extends App :
  // How to write the following?
  def sequence3(in: (Option[Int], Option[String], Option[Boolean])): Option[(Int, String, Boolean)] = ???

  //

  def matchOnList(l: List[Int]): Option[Int] = l match {
    case head :: tail => Some(head)
    case Nil => None
  }

  def matchOnTuple(t: Tuple): Option[Any] = t match {
    case head *: tail => Some(head)
    case EmptyTuple => None
  }

  println(matchOnTuple((10, "x", true)))
  println(matchOnTuple(("y", 14)))
  println(matchOnTuple(Tuple()))
  println("---")

  //

  (10, "x", true): (Int, String, Boolean)
  (10, "x", true): Int *: (String, Boolean)

  //

  type TupleMap[T <: Tuple, F[_]] <: Tuple = T match {
    case EmptyTuple => EmptyTuple
    case h *: t => F[h] *: TupleMap[t, F]
    }

  (Some(10), None, Some(true)): (Option[Int], Option[String], Option[Boolean])
  (Some(10), None, Some(true)): TupleMap[(Int, String, Boolean), Option]

  //

  def sequence[T <: Tuple](t: T): Option[InverseMap[T, Option]] = {
    val unwrapped = t.productIterator.collect { case Some(x) => x }.toArray[Any]
    if (unwrapped.length == t.productArity) Some(Tuple.fromArray(unwrapped).asInstanceOf[InverseMap[T, Option]]) else None
  }

  val test1: (Option[Int], Option[String], Option[Boolean]) = (Some(1), Some("x"), Some(true))
  val test2: (Option[Int], Option[String], Option[Boolean]) = (Some(1), None, Some(true))

  println(sequence(test1))
  println(sequence(test2))
  println("---")

  println(sequence((1, "x")))

  //

  def betterSequence[T <: Tuple](t: T)(using T <:< Map[InverseMap[T, Option], Option]): Option[InverseMap[T, Option]] = {
    val unwrapped = t.productIterator.collect { case Some(x) => x }.toArray[Any]
    if (unwrapped.length == t.productArity) Some(Tuple.fromArray(unwrapped).asInstanceOf[InverseMap[T, Option]]) else None
  }

  betterSequence(test1)

//betterSequence((1, "x"))


