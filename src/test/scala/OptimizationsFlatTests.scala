import org.junit.Test
import org.junit.Assert._
import optimisations.flat._
import cats.effect._

class OptimisationsFlatTest {

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
          s2 += (y + x) / 2 //checking if shadowing is not messing with me
        }
        _ <- IO.delay {
          assertEquals(s2.toLong, 42)
        }
      } yield ()
    }
    code.unsafeRunSync()
  }

  @Test def suspendingTesting(): Unit = {
    var s1 = 0
    var s2 = 0
    val code = optimise {
      for {
        x <- IO.pure(41)
        _ <- IO.delay(s1 += x)
        y <- IO.pure(43)
        _ <- IO.suspend {
          IO.delay {
           s2 += (y + x) / 2
          }
        }
      } yield ()
    }
    code.unsafeRunSync()
    assertEquals(s1.toLong, 41L)
    assertEquals(s2.toLong, 42L)
    code.unsafeRunSync()
    assertEquals(s1.toLong, 2 * 41L)
    assertEquals(s2.toLong, 2 * 42L)

  }

}