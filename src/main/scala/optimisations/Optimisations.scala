package optimisations

import cats.effect.IO

import scala.quoted._

inline def optimise[A](inline code: IO[A]) = ${ optimiseTop('code) }

def optimiseTop[A: Type](expr: Expr[IO[A]])(using QuoteContext): Expr[IO[A]] = {
  println(s"Code before optimisation:\n ${expr.show}")
  val optimised = optimiseIoMacro(expr)
  println(s"Generated optimised code:\n ${optimised.show}")
  optimised
}

def optimiseIoMacro[A: Type](expr: Expr[IO[A]])(using QuoteContext): Expr[IO[A]] = 
  expr match 
    case '{ (${io}: IO[$t]).flatMap[A]($f) } =>
      println(s"Optimising: ${io.show} flatMap <f>")
      optimiseFlatMapMacro(io, f)
    case '{ (${io}: IO[$t]).flatMap[$tt]($f).map[A]($ff) } =>
      println(s"Optimising: ${io.show} flatMap <f> map <ff>")
      '{ ${optimiseFlatMapMacro(io, f)}.map(${ff}.asInstanceOf[Any => A]) }
    case '{ (${io}: IO[$t]).map[A]($f) } =>
      println(s"Optimising: ${io.show} map <f>")
      optimiseMapMacro(io, f)
    case c =>
      summon[QuoteContext].warning(s"Failed to perform optimisations, got AST: ${c.show}", c)
      c
  


def optimiseFlatMapMacro[A: Type, B: Type](ioExpr: Expr[IO[A]], f: Expr[A => IO[B]])(using QuoteContext): Expr[IO[B]] =
  ioExpr match
    case '{ IO.pure[A]($v) } => 
      val singleStep = Expr.betaReduce(f)(v) 
      optimiseIoMacro(singleStep)
    case '{ IO.delay[A]($v) } => 
      '{ 
        IO.suspend { 
          val sideEffect: A = $v
          ${optimiseIoMacro(rewrapFunctions(Expr.betaReduce(f)('sideEffect)))} 
        }
      }
    case '{ IO.suspend[A]($nio) } => 
      val singleStep = optimiseIoMacro('{${nio}.flatMap($f)})
      '{ 
        IO.suspend {
          val sideEffect: IO[A] = $nio
          ${optimiseIoMacro('{ sideEffect.flatMap($f) })}
        }
      }
    case '{ (${io}: IO[$t]).map[A]($ff) } =>
      optimiseFlatMapMacro(io, '{ l => ${Expr.betaReduce(f)(Expr.betaReduce(ff)('l))} })
    case c => 
      println(s"Ast not changed:\n\n${c.show}")
      '{ ${c}.flatMap($f) }


def optimiseMapMacro[A: Type, B: Type](ioExpr: Expr[IO[A]], f: Expr[A => B])(using QuoteContext): Expr[IO[B]] = 
  ioExpr match
    case '{ IO.pure[A]($v) } => 
      val singleStep = Expr.betaReduce(f)(v)
      '{ IO.pure[B](${singleStep}) }
    case '{ IO.delay[A]($v) } => 
      val singleStep = Expr.betaReduce(f)(v)
      '{ IO.delay[B](${singleStep}) }
    case '{ IO.suspend[A]($nio) } => 
      val singleStep = optimiseIoMacro('{${nio}.map($f)})
      '{ IO.suspend(${singleStep}) }
    case c => 
      println(s"Ast not changed:\n\n${c.show}")
      '{ ${c}.map($f) }


def rewrapFunctions[A: Type](mess: Expr[IO[A]])(using QuoteContext): Expr[IO[A]] = 
  mess match
    case '{ (${sideEffect}: $t) match { case _ => $io } } =>
      io
    case c =>
      println(s"Fixing for-comprehension mess failed: ${c.show}")
      c
