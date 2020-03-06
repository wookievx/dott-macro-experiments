import cats.effect._
import cats.data.EitherT
import optimisations.flat.optimise

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
  // val multiMapProg = optimise {
  //   for {
  //     _ <- IO.delay("Failing")
  //     x = 44
  //     y = 45
  //     z = 46
  //     _ <- IO.delay(s"Killing: ${x + y + z}")
  //   } yield ()
  // }
  // multiMapProg.unsafeRunSync()
