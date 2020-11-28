package com.softwaremill.funscala

import scala.deriving.Mirror
import scala.compiletime.constValue
import scala.compiletime.erasedValue
import scala.compiletime.summonFrom
import scala.compiletime.summonInline
import scala.compiletime.summonAll

object S180_TypeclassDerivation extends App:
  trait SafeShow[T]:
    def show(t: T): String

  object SafeShow:
    inline def derived[T](using m: Mirror.Of[T]): SafeShow[T] = {
      val showElems: List[SafeShow[_]] = summonShowAll[m.MirroredElemTypes]
      inline m match {
        case s: Mirror.SumOf[T]     => ???
        case p: Mirror.ProductOf[T] => showProduct(p, showElems)
      }
    }

    private inline def summonShowAll[T <: Tuple]: List[SafeShow[_]] = inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[SafeShow[t]] :: summonShowAll[ts]
    }

    private inline def showProduct[T](p: Mirror.ProductOf[T], showElems: List[SafeShow[_]]): SafeShow[T] = {
      val label = constValue[p.MirroredLabel]
      val elemLabels = labelsToList[p.MirroredElemLabels]
      new SafeShow[T]:
        def show(t: T): String = {
          val elemValues = t.asInstanceOf[Product].productIterator.toList
          val elems = elemLabels.zip(elemValues).zip(showElems).map {
            case ((elLabel, el), safeShow) =>
              val shown = if (isSensitive(elLabel)) "***" else safeShow.asInstanceOf[SafeShow[Any]].show(el)
              elLabel + "=" + shown
          }.mkString(",")
          s"$label($elems)"
        }
    }

    private def isSensitive(n: String): Boolean = {
      val nn = n.toLowerCase
      nn.contains("token") || nn.contains("apikey") || nn.contains("password")
    }

    private inline def labelsToList[T <: Tuple]: List[String] = inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts) => constValue[t].toString :: labelsToList[ts]
    }

    given safeShowAny[T] as SafeShow[T] = new SafeShow[T]:
      def show(t: T) = t.toString

  extension[T] (t: T) {
    def safeShow(using SafeShow[T]): String = summon[SafeShow[T]].show(t)
  }

  case class Test1(f1: String, f2: Int) derives SafeShow
  case class Test2(token: String, tx: Long) derives SafeShow
  case class Test3(x: Int, t2: Test2) derives SafeShow

  println(Test1("x", 20).safeShow)
  println(Test2("xyz", 100L).safeShow)
  println(Test3(5, Test2("abc", 200L)).safeShow)
