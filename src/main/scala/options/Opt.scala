package options

type Opt[A] = A | Null

object Opt {

  import scala.quoted._

  def some[A](v: A): Opt[A] = v

  def none[A]: Opt[A] = null

  given ops as AnyRef {
    inline def [A, B](inline lhs: Opt[A]) map (inline rhs: A => B): Opt[B] = ${ mapMacro('lhs, 'rhs) }
    inline def [A, B](inline lhs: Opt[A]) flatMap (inline rhs: A => Opt[B]): Opt[B] = ${ flatMapMacro('lhs, 'rhs) }
  }

  inline def map[A, B](inline lhs: Opt[A], inline rhs: A => B): Opt[B] = ${ mapMacro('lhs, 'rhs) }
  inline def flatMap[A, B](inline lhs: Opt[A], inline rhs: A => Opt[B]): Opt[B] = ${ flatMapMacro('lhs, 'rhs) }

  private def mapMacro[A: Type, B: Type](lhs: Expr[Opt[A]], rhs: Expr[A => B])(using QuoteContext): Expr[Opt[B]] =
    val result = mapMacro_(lhs, rhs)
    println(s"Generated code: ${result.show}")
    result

  private def flatMapMacro[A: Type, B: Type](lhs: Expr[Opt[A]], rhs: Expr[A => Opt[B]])(using QuoteContext): Expr[Opt[B]] =
    val result = flatMapMacro_(lhs, rhs)
    println(s"Generated code: ${result.show}")
    result

  private def mapMacro_[A: Type, B: Type](lhs: Expr[Opt[A]], rhs: Expr[A => B])(using QuoteContext): Expr[Opt[B]] =
    lhs match
      case '{ Opt.none[A] } =>
        '{ Opt.none[B] }
      case '{ Opt.some[A]($v) } =>
        Expr.betaReduce(rhs)(v)
      case '{ $v: Opt[A] } =>
        '{
          val result = $v
          if result != null
            val hack: A = result
            ${Expr.betaReduce(rhs)('hack)}
          else
            Opt.none[B]
        }

  private def flatMapMacro_[A: Type, B: Type](lhs: Expr[Opt[A]], rhs: Expr[A => Opt[B]])(using QuoteContext): Expr[Opt[B]] =
    lhs match
      case '{ Opt.none[A] } =>
        '{ Opt.none[B] }
      case '{ Opt.some[A]($v) } =>
        Expr.betaReduce(rhs)(v)
      case '{ $v: Opt[A] } =>
        '{
          val result = $v
          if result != null
            val hack: A = result
            ${ Expr.betaReduce(rhs)('hack) }
          else
            Opt.none[B]
        }
  
}
