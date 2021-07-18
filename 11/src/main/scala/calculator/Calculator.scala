package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator extends CalculatorInterface {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map {case (k,v ) => k -> Signal(eval(v(), namedExpressions))}
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case x: Literal => x.v
      case x: Ref => eval(getReferenceExpr(x.name, references), references.removed(x.name))
      case x: Plus => eval(x.a, references) + eval(x.b, references)
      case x: Minus => eval(x.a, references) - eval(x.b, references)
      case x: Times => eval(x.a, references) * eval(x.b, references)
      case x: Divide => if (x.b == 0) Double.NaN else eval(x.a, references) / eval(x.b, references)
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
