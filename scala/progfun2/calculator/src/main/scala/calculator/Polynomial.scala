package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
    c: Signal[Double]): Signal[Double] = {
    // bb - 4ac
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
    c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      delta() match {
        case d if d < 0 => Set()
        case d => Set((-b() + math.sqrt(d)) / (2 * a()), (-b() - math.sqrt(d)) / (2 * a()))
      }
    }
  }
}
