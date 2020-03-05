import cats.effect._
import optimisations.optimise

@main def foo(name: String): Unit = 
  println(s"Hello $name")
  val advanvedProg = optimise {
    for {
      x <- IO.pure(44)
      _ <- IO.delay(println(s"Got: $x"))
      y <- IO.pure(42)
      _ <- IO.delay(println(s"Got: $y"))
    } yield x + y
  }
  advanvedProg.unsafeRunSync
