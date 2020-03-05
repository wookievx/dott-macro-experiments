package optimisations.flat

import cats.effect.IO
import optimisations.rewrapFunctions
import scala.quoted._

inline def optimise[A](inline code: IO[A]) = ${ optimiseTop('code) }

def optimiseTop[A: Type](expr: Expr[IO[A]])(using QuoteContext): Expr[IO[A]] =
  println(s"Code before optimisation:\n ${expr.show}")
  val optimised = optimiseIoMacro(expr)
  optimised match
    case Right(code) =>
      println(s"Generated optimised code:\n ${code.show}")
      code
    case Left(v) =>
      println(s"Generated optimised code:\n ${v.show}")
      '{ IO.delay($v) }
  

def optimiseIoMacro[A: Type](expr: Expr[IO[A]])(using QuoteContext): Either[Expr[A], Expr[IO[A]]] = 
  expr match 
    case '{ (${io}: IO[$t]).flatMap[A]($f) } =>
      println("Optimising: <io> flatMap <f>")
      optimiseFlatMapMacro(io, f)
    case '{ (${io}: IO[$t]).flatMap[$tt]($f).map[A]($ff) } =>
      println("Optimising: <io> flatMap <f> map <ff>")
      optimiseFlatMapMacro(io, f).fold(
        raw => Left('{
          val unpacked = $raw
          ${Expr.betaReduce(ff)('unpacked)}
        }),
        code => optimiseMapMacro(code, ff)
      )
    case '{ (${io}: IO[$t]).map[A]($f) } =>
      println("Optimising: <io> map <f>")
      optimiseMapMacro(io, f)
    case c =>
      summon[QuoteContext].warning(s"Failed to perform optimisations, got AST: ${c.show}", c)
      Right(c)

def optimiseFlatMapMacro[A: Type, B: Type](ioExpr: Expr[IO[A]], f: Expr[A => IO[B]])(using QuoteContext): Either[Expr[B], Expr[IO[B]]] = ???

def optimiseMapMacro[A: Type, B: Type](ioExpr: Expr[IO[A]], f: Expr[A => B])(using QuoteContext): Either[Expr[B], Expr[IO[B]]] = ???
