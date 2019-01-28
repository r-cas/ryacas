
# Demo of the Expr interface

x <- Expr(expression(x))
x*x*x
deriv(x^3 + cos(x^2), x)

