package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    val bSquare = Signal(b()*b())

    Signal{
      bSquare() - 4*a()*c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

      val nB = Signal(-1 * b())
      val aa = Signal(2 * a())
      val sqrtDelta = Signal(math.sqrt(delta()))

      Signal {
        if (delta() < 0) Set()
        else {
          Set(
            (nB() + sqrtDelta())/ aa(),
            (nB() - sqrtDelta())/ aa()
          )
        }
      }
    }
}
