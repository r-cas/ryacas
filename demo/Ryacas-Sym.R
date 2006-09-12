
# Demo of the Sym interface

x <- Sym("x")
sym0 <- x^3
sym1 <- x^2+2*x^2
sym2 <- 2 * sym0
sym3 <- Sym(6) * pi * x
sym4 <- sym1 * (1 - Sin(sym3)) / sym2
print(sym4)
sym5 <- Simplify(sym4) 
print(sym5)
x. <- -3 + (0:600)/300
plot(x., Eval(sym5, list(x = x.)), type = "l", col = "red")
lines(x., .1 + Eval(sym5, list(x = x.)), type = "l", col = "blue")

sym6 <- Deriv(Sin(x), x)
print(Factorial(10))
print(x^Seq(3))
print(Deriv(List(Cos(x), Sin(x)), x))
print(Exp(1))
print(N(Sin(1)^2+Cos(x)^2))
print(Integrate(Tan(x), x, Sym("q"), Pi/(12 %/% 3)))
print(Deriv(Sin(x), x))
y <- Sym("y")
print(Deriv(Cos(x)+Sin(y), List(x,y)))
print(Integrate(Sin(x), x))
print(Integrate(Sin(x), x, 0, Pi))

