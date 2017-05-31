trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
case class Var(n: String) extends Expr

def show(e: Expr): String = e match {
  case Number(n) => n.toString
  case Sum(x, y) => show(x) + " + " + show(y)
  case Prod(x:Sum, y) => "(" + show(x) + ") * " + show(y)
  case Prod(x, y) => show(x) + " * " + show(y)
  case Var(x) => x
}


println(show(Sum(Number(1), Number(100))))
println(show(Sum(Prod(Number(2), Var("x")), Var("y"))))
println(show(Prod(Sum(Number(2), Var("x")), Var("y"))))
println(show(Sum(Prod(Number(2), Prod(Sum(Number(2), Number(2)), Number(2))), Number(2))))
