
# Demo of the Sym interface

x <- Sym("x")
sym0 <- x^3
sym1 <- x^2+2*x^2
sym2 <- 2 * sym0
sym3 <- Sym(6) * pi * x
sym4 <- sym1 * (1 - sin(sym3)) / sym2
print(sym4)
sym5 <- Simplify(sym4) 
print(sym5)
x. <- -3 + (0:600)/300
plot(x., Eval(sym5, list(x = x.)), type = "l", col = "red")
lines(x., .1 + Eval(sym5, list(x = x.)), type = "l", col = "blue")

sym6 <- deriv(sin(x), x)
print(Factorial(Sym(10)))
print(deriv(List(cos(x), sin(x)), x))
print(exp(Sym(1)))
print(N(sin(1)^2+cos(x)^2))
print(Integrate(tan(x), x, Sym("q"), Pi/(12 %/% 3)))
print(deriv(sin(x), x))
y <- Sym("y")
print(deriv(cos(x)+sin(y), List(x,y)))
print(Integrate(sin(x), x))
print(Integrate(sin(x), x, 0, Pi))


