package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(scala.math.pow(b(),2) - (4 * a() * c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    def set = if (delta() < 0) Set[Double]() else
      Set(((0 - b() + scala.math.sqrt(delta()))/ (2 * a())), ((0 - b() - math.sqrt(delta())) / (2 * a())))

    Signal(set)

  }
}
