package dp181e

object Equations {
  case class Equation(slope: Double, offset: Double) {
    def intersect(e2: Equation): Option[(Double, Double)] =
      if (slope == e2.slope && offset != e2.offset) None
      else Some((-(offset - e2.offset) / (slope - e2.slope), -(offset - e2.offset) / (slope - e2.slope) * slope + offset))
  }

  object Equation {
    def parse(s: String): Option[Equation] = {
      val equationRegex = """(?:y=)?([+-]?\d+(?:[.]\d+)?)x([+-]\d+(?:[.]\d+)?)?""".r
      s.replaceAll("\\s+", "") match {
        case equationRegex(slope, null) => Some(Equation(slope.toDouble, 0))
        case equationRegex(slope, offset) => Some(Equation(slope.toDouble, offset.toDouble))
        case _ => None
      }
    }
  }
}
