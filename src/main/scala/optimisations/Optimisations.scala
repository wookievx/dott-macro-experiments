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
      println("Optimising: <io> flatMap <f>")
      optimiseFlatMapMacro(io, f)
    case '{ (${io}: IO[$t]).flatMap[$tt]($f).map[A]($ff) } =>
      println("Optimising: <io> flatMap <f> map <ff>")
      '{ ${optimiseFlatMapMacro(io, f)}.map(${ff}.asInstanceOf[Any => A]) }
    case '{ (${io}: IO[$t]).map[A]($f) } =>
      println("Optimising: <io> map <f>")
      optimiseMapMacro(io, f)
    case c =>
      summon[QuoteContext].warning(s"Failed to perform optimisations, got AST: ${c.show}", c)
      c
  }


def optimiseFlatMapMacro[A: Type, B: Type](ioExpr: Expr[IO[A]], f: Expr[A => IO[B]])(using QuoteContext): Expr[IO[B]] =
  ioExpr match
    case '{ IO.pure[A]($v) } => 
      val singleStep = Expr.betaReduce(f)(v) 
      println(s"Optimising pure case result:\n\n${singleStep.show}")
      optimiseIoMacro(singleStep)
    case '{ IO.delay[A]($v) } => 
      val singleStep = Expr.betaReduce(f)(v)
      println(s"Optimising delay case result:\n\n${singleStep.show}")
      '{ 
        IO.suspend { 
          val sideEffect: A = $v
          ${optimiseIoMacro(rewrapFunctions('{($f)(sideEffect)}))} 
        }
      }
    case '{ IO.suspend[A]($nio) } => 
      val singleStep = optimiseIoMacro('{${nio}.flatMap($f)}) 
      println(s"Suspend case result:\n\n${singleStep.show}")
      '{ 
        IO.suspend {
          val sideEffect: IO[A] = $nio
          ${optimiseIoMacro('{ sideEffect.flatMap($f) })}
        }
      }
    case c => 
      println("Ast not changed")
      '{ ${c}.flatMap($f) }


def optimiseMapMacro[A: Type, B: Type](ioExpr: Expr[IO[A]], f: Expr[A => B])(using QuoteContext): Expr[IO[B]] = 
  ioExpr match
    case '{ IO.pure[A]($v) } => 
      val singleStep = Expr.betaReduce(f)(v)
      println(s"Optimising pure case result:\n\n${singleStep.show}")
      '{ IO.pure[B](${singleStep}) }
    case '{ IO.delay[A]($v) } => 
      val singleStep = Expr.betaReduce(f)(v)
      println(s"Optimising delay case result:\n\n${singleStep.show}")
      '{ IO.delay[B](${singleStep}) }
    case '{ IO.suspend[A]($nio) } => 
      val singleStep = optimiseIoMacro('{${nio}.map($f)})
      println(s"Optimising suspend case result:\n\n${singleStep.show}")
      '{ IO.suspend(${singleStep}) }
    case c => 
      println("Ast not changed")
      '{ ${c}.map($f) }


def rewrapFunctions[A: Type](mess: Expr[IO[A]])(using QuoteContext): Expr[IO[A]] = 
  mess match {
    case '{ (${f}: $t => IO[A]).apply($arg) } =>
      val partialRewrite = Expr.betaReduce(f)(arg)
      partialRewrite match {
        case '{ (${sideEffect}: $t) match { case _ => $io } } =>
          io
        case c =>
          println(s"Fixing for-comprehension mess failed: ${c.show}")
          c
      }
    case c => c
  }