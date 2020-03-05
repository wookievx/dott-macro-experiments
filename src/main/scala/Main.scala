import cats.effect._
import optimisations.optimise

@main def foo(name: String): Unit = 
  println(s"Hello $name")
  val exampleProg = optimise {
    for {
      x <- IO.pure(22)
      y <- IO.pure(44)
      _ <- IO.delay(println(s"Calculated: ${x + y}"))
    } yield x + y
  }
  exampleProg.unsafeRunSync()
  val advanvedProg = optimise {
    for {
      x <- IO.pure(44)
      _ <- IO.delay(println("Got: $x"))
      y <- IO.pure(42)
      _ <- IO.delay(println("Got: $y"))
    } yield x + y
  }
  advanvedProg.unsafeRunSync
