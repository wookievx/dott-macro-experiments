import cats.effect._
import cats.data.EitherT
import optimisations.flat.optimise
import options._
import options.Opt.{given _}

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
  val somePeskyNullable: Opt[Int] = 42

  val generateIfs = for {
    x1 <- somePeskyNullable
    x2 <- if x1 > 42 then Opt.some(0) else Opt.some(x1)
    x3 <- if x1 < 42 && x2 > 34 then Opt.some(9000) else Opt.some(0)
  } yield x3
  println(generateIfs)

  val optimisiedAway = for {
    x1 <- Opt.some(44)
    x2 <- Opt.some(66)
  } yield (x1 + x2) / 2
  println(optimisiedAway)
