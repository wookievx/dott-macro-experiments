package cats.effect.opts

import cats.effect.IO

enum OptIO[+A] with
  case Pure[+A](a: A) extends OptIO[A]
  case Delay[+A](thunk: () => A) extends OptIO[A]
  case Suspend[+A](thunk: () => OptIO[A]) extends OptIO[A]
  case Map[A, +B](source: OptIO[A], f: A => B) extends OptIO[B]
  case Bind[A, +B](source: OptIO[A], f: A => OptIO[B]) extends OptIO[B]
  case Delegate[+A](impl: IO[A]) extends OptIO[A]

  inline def map[B](f: A => B) <: OptIO[B] = Map[A, B](this, f)

object OptIO with
  inline def pure[A](a: A) <: OptIO[A] = Pure(a)
  inline def delay[A](body: => A) <: OptIO[A] = Delay(() => body)
  inline def delegate[A](impl: IO[A]) <: OptIO[A] = Delegate(impl)
  inline def suspend[A](body: => OptIO[A]) <: OptIO[A] = Suspend(() => body)

  inline def [A, B](inline lhs: OptIO[A]) flatMap (inline rhs: A => OptIO[B]) = ${ flatMapMacro('lhs, 'rhs) }

  import scala.quoted._

  def flatMapMacro[A: Type, B: Type](io: Expr[OptIO[A]], f: Expr[A => OptIO[B]])(using QuoteContext): Expr[OptIO[B]] = 
    io match
      case '{ OptIO.Pure.apply[A]($v) } =>
        Expr.betaReduce(f)(v)        
      case '{ OptIO.Delay.apply[A]($body) }  =>
        '{ Suspend(() => ${Expr.betaReduce(f)('{($body)()})})}
      case '{ OptIO.delay($body) } =>
        '{ Suspend(() => ${Expr.betaReduce(f)(body)})}
      case c =>
        summon[QuoteContext].warning(s"Expanding via Bind, got ast: ${c.show}", c)
        '{ Bind($io, $f) }
    

    

  
