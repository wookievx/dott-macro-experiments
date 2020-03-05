package cats.effect.opts

import cats.effect.IO

enum OptIO[+A] with
  case Pure[+A](a: A) extends OptIO[A]
  case Delay[+A](thunk: () => A) extends OptIO[A]
  case Suspend[+A](thunk: () => OptIO[A]) extends OptIO[A]
  case Map[A, +B](source: OptIO[A], f: A => B) extends OptIO[B]
  case Bind[A, +B](source: OptIO[A], f: A => OptIO[B]) extends OptIO[B]
  case Delegate[+A](impl: IO[A]) extends OptIO[A]


object OptIO with
  def pure[A](a: A): OptIO[A] = Pure(a)
  def delay[A](body: => A): OptIO[A] = Delay(() => body)
  def delegate[A](impl: IO[A]): OptIO[A] = Delegate(impl)
  def suspend[A](body: => OptIO[A]): OptIO[A] = Suspend(() => body)

  inline def [A, B](inline lhs: OptIO[A]) flatMap (inline rhs: A => OptIO[B]) = ${ flatMapMacro('lhs, 'rhs) }
  inline def [A, B](inline lhs: OptIO[A]) map (inline rhs: A => B) = ${ mapMacro('lhs, 'rhs) }

  inline def [A](inline optIO: OptIO[A]).toIO = ${ toIOMacro('optIO) }

  inline def optimise[A](inline optIO: OptIO[A]) <: IO[A] = ${ toIOMacro('optIO) }

  import scala.quoted._

  def flatMapMacro[A: Type, B: Type](io: Expr[OptIO[A]], f: Expr[A => OptIO[B]])(using QuoteContext): Expr[OptIO[B]] = 
    io match
      case '{ OptIO.Pure.apply[A]($v) } =>
        Expr.betaReduce(f)(v) 
      case '{ OptIO.pure[A]($v) } =>
          Expr.betaReduce(f)(v)        
      case '{ OptIO.Delay.apply[A]($body) }  =>
        '{ OptIO.suspend(${Expr.betaReduce(f)('{($body)()})})}
      case '{ OptIO.delay[A]($body) } =>
        '{ OptIO.suspend(${Expr.betaReduce(f)(body)}) }
      case '{ OptIO.suspend[A]($body) } =>
        '{ OptIO.suspend(${flatMapMacro(body, f)})}
      case c =>
        summon[QuoteContext].warning(s"Expanding via Bind, got ast: ${c.show}", c)
        '{ Bind($io, $f) }

  def mapMacro[A: Type, B: Type](io: Expr[OptIO[A]], f: Expr[A => B])(using QuoteContext): Expr[OptIO[B]] =
    io match
      case '{ OptIO.Pure.apply[A]($v) } =>
        '{ OptIO.pure[B](${Expr.betaReduce(f)(v)}) }
      case '{ OptIO.pure[A]($v) } =>
        '{ OptIO.pure[B](${Expr.betaReduce(f)(v)}) }
      case '{ OptIO.delay[A]($body) } =>
        '{ OptIO.delay[B](${Expr.betaReduce(f)(body)}) }
      case '{ OptIO.suspend[A]($body) } =>
        '{ OptIO.suspend[B](${mapMacro(body, f)}) }
      case c =>
        summon[QuoteContext].warning(s"Expanding via Map, got ast: ${c.show}", c)
        '{ Map($io, $f) }

  def runtimeToIO[A](optIO: OptIO[A]): IO[A] = optIO match
    case Pure(v) => IO.pure(v)
    case Delay(v) => IO.Delay(v)
    case Suspend(v) => IO.Suspend(() => runtimeToIO(v()))
    case Delegate(v) => v
    case Map(v, f) => runtimeToIO(v).map(f)
    case Bind(v, f) => runtimeToIO(v).flatMap(v => runtimeToIO(f(v)))

  def toIOMacro[A: Type](io: Expr[OptIO[A]])(using QuoteContext): Expr[IO[A]] =
    io match
      case '{ OptIO.Pure.apply[A]($v) } => '{ IO.pure($v) }
      case '{ OptIO.pure[A]($v) } => '{ IO.pure($v) }
      case '{ OptIO.Delay.apply[A]($body) } => '{ IO.Delay($body) }
      case '{ OptIO.delay[A]($v) } => '{ IO.delay($v) }
      case '{ OptIO.Suspend.apply[A]($body) } => 
        '{ IO.Suspend(() => ${toIOMacro(Expr.betaReduce(body)())}) }
      case '{ OptIO.suspend[A]($body) } =>
        '{ IO.suspend(${toIOMacro(body)}) }
      case c =>
        summon[QuoteContext].warning(s"Converting in runtime, got ast: ${c.show}", c)
        '{ runtimeToIO($c) }
    
  
