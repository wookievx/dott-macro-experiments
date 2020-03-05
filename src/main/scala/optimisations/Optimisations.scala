package optimisations

import cats.effect.IO

import scala.quoted._

inline def optimise[A](inline code: IO[A]) = ${ optimiseTop('code) }

def optimiseTop[A: Type](expr: Expr[IO[A]])(using QuoteContext): Expr[IO[A]] = {
  val optimised = optimiseIoMacro(expr)
  println(s"Generated optimised code: ${optimised.show}")
  optimised
}

def optimiseIoMacro[A: Type](expr: Expr[IO[A]])(using QuoteContext): Expr[IO[A]] = 
  expr match {
    case '{ (${io}: IO[$t]).flatMap[A]($f) } =>
      println(s"Optimising flatMap for: ${io.show}, ${f.show}")
      optimiseFlatMapMacro(io, f)
    case '{ (${io}: IO[$t]).flatMap[$tt]($f).map[A]($ff) } =>
      println(s"Optimising flatMap for: ${io.show}, ${f.show}, ${ff.show}")
      '{ ${optimiseFlatMapMacro(io, f)}.map(${ff}.asInstanceOf[Any => A]) }
    case '{ (${io}: IO[$t]).map[A]($f) } =>
      println(s"Optimising map for: ${io.show}, ${f.show}")
      optimiseMapMacro(io, f)
    case c =>
      summon[QuoteContext].warning(s"Failed to perform optimisations, got AST: ${c.show}", c)
      c
  }


def optimiseFlatMapMacro[A: Type, B: Type](ioExpr: Expr[IO[A]], f: Expr[A => IO[B]])(using QuoteContext): Expr[IO[B]] =
  ioExpr match
    case '{ IO.pure[A]($v) } => 
      println("Optimising pure case")
      optimiseIoMacro(Expr.betaReduce(f)(v))
    case '{ IO.delay[A]($v) } => 
      println("Optimising delay case")
      '{ IO.suspend(${optimiseIoMacro(Expr.betaReduce(f)(v))}) }
    case '{ IO.suspend[A]($nio) } => 
      println("Optimising suspend case")
      '{ IO.suspend(${optimiseIoMacro('{${nio}.flatMap($f)})}) }
    case c => 
      println("Ast not changed")
      '{ ${c}.flatMap($f) }


def optimiseMapMacro[A: Type, B: Type](ioExpr: Expr[IO[A]], f: Expr[A => B])(using QuoteContext): Expr[IO[B]] = 
  ioExpr match
    case '{ IO.pure[A]($v) } => 
      println("Optimising pure case")
      '{ IO.pure[B](${Expr.betaReduce(f)(v)}) }
    case '{ IO.delay[A]($v) } => 
      println("Optimising delay case")
      '{ IO.delay[B](${Expr.betaReduce(f)(v)}) }
    case '{ IO.suspend[A]($nio) } => 
      println("Optimising suspend case")
      '{ IO.suspend(${optimiseIoMacro('{${nio}.map($f)})}) }
    case c => 
      println("Ast not changed")
      '{ ${c}.map($f) }

