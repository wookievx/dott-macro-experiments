package optimisations.flat

import cats.effect.IO
import optimisations.rewrapFunctions
import scala.quoted._

inline def optimise[A](inline code: IO[A]) = ${ optimiseTop('code) }

def optimiseTop[A: Type](expr: Expr[IO[A]])(using QuoteContext): Expr[IO[A]] =
  println(s"Code before optimisation:\n ${expr.show}")
  val optimised = optimiseIoMacro(expr)
  println(s"Generated optimised code:\n ${optimised.show}")
  optimised match {
    case '{ ${io}: IO[A] } => io
    case '{ ${code}: A } => '{ IO.delay($code) }
  }
  

def optimiseIoMacro[A: Type](expr: Expr[IO[A]])(using QuoteContext): Expr[A | IO[A]] = 
  expr match 
    case '{ (${io}: IO[$t]).flatMap[A]($f) } =>
      println("Optimising: <io> flatMap <f>")
      optimiseFlatMapMacro(io, f)
    case '{ (${io}: IO[$t]).flatMap[$tt]($f).map[A]($ff) } =>
      println("Optimising: <io> flatMap <f> map <ff>")
      optimiseSpecial(io, f, ff)
    case '{ (${io}: IO[$t]).map[A]($f) } =>
      println("Optimising: <io> map <f>")
      optimiseMapMacro(io, f)
    case '{ $v: A } =>
        v
    case c =>
      summon[QuoteContext].warning(s"Failed to perform optimisations, got AST: ${c.show}", c)
      c

def optimiseSpecial[A: Type, B: Type, C: Type](io: Expr[IO[A]], f: Expr[A => IO[B]], ff: Expr[B => C])(using QuoteContext): Expr[C | IO[C]] = 
  optimiseFlatMapMacro(io, f) match
    case '{ ${io}: IO[B] } =>
      optimiseMapMacro(io, ff)
    case '{ ${c}: B } =>
      Expr.betaReduce(ff)(c)

def wrapUnion[A: Type](unionExpr: Expr[A | IO[A]])(using QuoteContext): Expr[IO[A]] = 
  unionExpr match
    case '{ ${io}: IO[A] } => io
    case '{ ${code}: A } => '{ IO.delay($code) }

def optimiseFlatMapMacro[A: Type, B: Type](ioExpr: Expr[IO[A]], f: Expr[A => IO[B]])(using QuoteContext): Expr[B | IO[B]] =
  ioExpr match
    case '{ IO.pure[A]($v) } => 
      '{
        val memoized = $v
        ${optimiseIoMacro(Expr.betaReduce(f)('memoized))}
      }
    case '{ IO.delay[A]($v) } =>
      '{
        val memoized = $v
        ${optimiseIoMacro(rewrapFunctions(Expr.betaReduce(f)('memoized)))}
      }
    case '{ IO.suspend[A]($nio) } => 
      val res: Expr[B | IO[B]] = '{
        val memoized = $nio
        memoized.flatMap(arg => ${wrapUnion(optimiseIoMacro(rewrapFunctions(Expr.betaReduce(f)('arg))))})
      }
      res match 
        case '{ ${io}: IO[B] } =>
          '{ IO.suspend[B]($io) }
        case _ =>
          res  
    case '{ (${io}: IO[$t]).map[A]($ff) } =>
      optimiseFlatMapMacro(io, '{ l => ${Expr.betaReduce(f)(Expr.betaReduce(ff)('l))} })
    case c => 
      println(s"Ast not changed:\n\n${c.show}\n${f.show}")
      '{ ${c}.flatMap($f) }

def optimiseMapMacro[A: Type, B: Type](ioExpr: Expr[IO[A]], f: Expr[A => B])(using QuoteContext): Expr[B | IO[B]] = 
  ioExpr match
    case '{ IO.pure[A]($v) } => 
      val singleStep = Expr.betaReduce(f)(v)
      println(s"Optimising pure case result:\n\n${singleStep.show}")
      '{ 
        val x = $v
        ${Expr.betaReduce(f)('x)}
       }
    case '{ IO.delay[A]($v) } => 
      '{ 
        val memoized = $v
        ${Expr.betaReduce(f)('memoized)}
      }
    case '{ IO.suspend[A]($nio) } => 
      '{ 
        val memoized = $nio
        ${optimiseIoMacro('{memoized.map($f)})} 
      }
    case c => 
      println(s"Ast not changed:\n\n${c.show}\n${f.show}")
      '{ ${c}.map($f) }

def rewrapFunctions[A: Type](mess: Expr[IO[A]])(using QuoteContext): Expr[IO[A]] = 
  mess match
    case '{ (${sideEffect}: $t) match { case _ => $io } } =>
      io
    case c =>
      println(s"Fixing for-comprehension mess failed: ${c.show}")
      c
