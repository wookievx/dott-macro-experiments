import cats.effect._
import cats.effect.opts._

@main def foo(name: String): Unit = 
  import OptIO.flatMap
  val prog = for {
    x1 <- OptIO.Delay { () =>
      println(s"I am working as expected: 41")
      41
    }
    x2 <- OptIO.Delay { () =>
      println(s"I am working as expected: 43")
      43
    }
  } yield (x1 + x2) / 2
  println(s"I am evaluating first")
  println(prog) //optymalizuje mi do pojedynczego OptIO.Suspend(() => ...)
  val secondProg = OptIO.pure(44).flatMap(x => OptIO.delay(println(s"Got: $x")))
  println(secondProg)
