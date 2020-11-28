package com.softwaremill.funscala

import scala.quoted.{Expr, QuoteContext, Type}

object S190_Macro:
  inline def timed[T](inline expr: T): T = ${timedImpl('expr)}

  private def timedImpl[T: Type](expr: Expr[T])(using QuoteContext): Expr[T] =
    '{
      val start = System.currentTimeMillis()
      try $expr
      finally {
        val end = System.currentTimeMillis()
        val exprAsString = ${Expr(expr.show)}.replaceAll("\\s+", " ").trim()
        val exprAsStringShort = if (exprAsString.length > 50) exprAsString.take(50)+"..." else exprAsString
        println(s"Evaluating $exprAsStringShort took: ${end-start}ms")
      }
    }

