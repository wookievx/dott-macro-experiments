import org.junit.Test
import org.junit.Assert._

import options._
import options.Opt.{given _}

class OptTest {

  @Test def trivialSomeSpec(): Unit = {
    val code = for {
      x1 <- Opt.some(41)
      x2 <- Opt.some(43)
    } yield (x1 + x2) / 2
    
    code match 
      case i: Int =>
        assertEquals(42, i.asInstanceOf[Long])
      case _ =>
        assert(false, "Result is null when should be defined")
  }
  
  @Test def trivialNoneSpec(): Unit = {
    var didSidefect: Boolean = false
    
    val code = for {
      x1 <- Opt.none[Int]
      x2 <- Opt.some {
        didSidefect = true
        1000
      }
    } yield (x1 + x2) / 2
    
    assertFalse(didSidefect)
    assertNull(code)
  }
  
  @Test def nonTrivialSomeSpec(): Unit = {
    val somePeskyNullable: Opt[Int] = 42

    val code = for {
      x1 <- somePeskyNullable
      x2 <- if x1 > 42 then Opt.some(0) else Opt.some(x1)
      x3 <- if x2 > 34 then Opt.some(9000) else Opt.some(0)
    } yield x3
    
    code match 
      case i: Int =>
        assertEquals(9000, i.toLong)
      case _ =>
        assert(false, "Result is null when should be defined")
    
  }
  
}
