import cats.effect._
import cats.effect.opts._

@main def foo(name: String): Unit = 
  import OptIO._

  val prog = for {
    x1 <- OptIO.delay {
      println(s"I am working as expected: 41")
      41
    }
    x2 <- OptIO.delay {
      println(s"I am working as expected: 43")
      43
    }
  } yield (x1 + x2) / 2
  println(prog) //optimising to singl Suspend(() => ...)
  
  val execProg = optimise {
    for {
      x1 <- OptIO.delay {
        println(s"I am working as expected: 41")
        41
      }
      x2 <- OptIO.delay {
        println(s"I am working as expected: 43")
        43
      }
    } yield (x1 + x2) / 2
  }

  execProg.unsafeRunSync()

  val secondProg = OptIO.pure(44).flatMap(x => OptIO.delay(println(s"Got: $x")))
  println(secondProg)
  secondProg.toIO.unsafeRunSync()
