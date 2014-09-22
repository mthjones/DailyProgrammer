package dp181e

object Equations {
  case class Equation(slope: Double, offset: Double) {
    def intersect(e2: Equation): Option[(Double, Double)] =
      if (slope == e2.slope && offset != e2.offset) None
      else Some((-(offset - e2.offset) / (slope - e2.slope), -(offset - e2.offset) / (slope - e2.slope) * slope + offset))
  }

  object Equation {
    def parse(s: String): Option[Equation] = {
      val equationRegex = """(?:\s*y\s*=\s*)?([+-]?\d+(?:[.]\d+)?)x\s*([+-]\s*\d+(?:[.]\d+)?)?""".r
      s match {
        case equationRegex(slope, offset) =>
          Some(Equation(slope.toDouble, if (offset == null) 0 else offset.replaceAll("\\s+", "").toDouble))
        case _ => None
      }
    }
  }
}
