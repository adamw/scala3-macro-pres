package com.softwaremill.funscala

import scala.quoted.{Expr, Quotes, Type}

object S190_Macro:
  inline def timed[T](inline expr: T): T = ${timedImpl('expr)}

  private def timedImpl[T: Type](expr: Expr[T])(using Quotes): Expr[T] =
    '{
      val start = System.currentTimeMillis()
      try $expr
      finally {
        val end = System.currentTimeMillis()
        val exprAsString = ${Expr(exprAsCompactString(expr))}.replaceAll("\\s+", " ").trim()
        val exprAsStringShort = if (exprAsString.length > 50) exprAsString.take(50)+"..." else exprAsString
        // ${Expr(expr.show)}.replaceAll("\\s+", " ").trim()
        println(s"Evaluating $exprAsStringShort took: ${end-start}ms")
      }
    }
    
  private def exprAsCompactString[T: Type](expr: Expr[T])(using ctx: Quotes): String = {
    import ctx.reflect._
    expr.asTerm match {
      case Inlined(_, _, Apply(method, params)) => s"${method.symbol.name}(${params.map(_.show).mkString(",")})"
      case _ => expr.show
    }   
  }

