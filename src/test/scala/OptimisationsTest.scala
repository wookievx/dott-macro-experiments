import org.junit.Test
import org.junit.Assert._
import optimisations._
import cats.effect._

class OptimisationsTest {

  @Test def sideEffectTesting(): Unit = {
    var s1 = 0
    var s2 = 0
    val code = optimise {
      for {
        x <- IO.pure(41)
        _ <- IO.delay {
          assertEquals(s2.toLong, 0l)
          s1 += x
        }
        y <- IO.pure(43)
        _ <- IO.delay {
          assertEquals(s1.toLong, 41)
          s2 += y
        }
        _ <- IO.delay {
          assertEquals(s2.toLong, 43)
        }
      } yield ()
    }
    code.unsafeRunSync()
  }

}