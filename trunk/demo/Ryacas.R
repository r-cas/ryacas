	x <- -3 + (0:600)/300


	exp0 <- expression(x ^ 3)
	exp1 <- expression(x^2 + 2 * x^2)
	exp2 <- expression(2 * exp0)
	exp3 <- expression(6 * pi * x)
	exp4 <- expression((exp1 * (1 - sin(exp3))) / exp2)
	res1 <- yacas(exp4); print(res1)

	exp5 <- expression(Simplify(exp4))
	res2 <- yacas(exp5); print(res2)

	plot(x, eval(res2[[1]]), type="l", col="red")
	points(x, .1 + eval(res2[[1]]), type="l", col="blue")

	exp6 <- expression(deriv(sin, x))
	print(yacas(exp6))

	print(yacas(expression(factorial(10))))
	res5 <- yacas(expression(deriv(asin,"x"))); print(res5)
	res6 <- yacas(expression(x^(1:3))); print(res6)
	res7 <- yacas(expression(deriv(list(sin(x), cos(x)), x))); print(res7)
	res8 <- yacas(expression(exp(1))); print(res8)
	print(yacas(expression(limit(x,0,sin(x)/x))))
	
	print(yacas(expression(N(sin(1)^2+cos(x)^2))))

	
	print(yacas(expression(integrate(tan, q, pi/(12%/%3)))))

	# various forms of deriv

	print(yacas(expression(deriv(sin))))
	print(yacas(expression(deriv(sin, x))))
	print(yacas(expression(deriv(sin, "x"))))
	print(yacas(expression(deriv(sin(x))))) # x is the default
	print(yacas(expression(deriv(sin(x), x))))
	print(yacas(expression(deriv(sin(x), "x"))))
	print(yacas(expression(deriv(cos(x)+sin(x), x))))
	print(yacas(expression(deriv(cos(x)+sin(y), list(x, y)))))
	print(yacas(expression(deriv(cos(x)+sin(y), list("x", "y")))))
	print(yacas(expression(deriv(cos(x)+sin(y), c(x, y)))))
	print(yacas(expression(deriv(cos(x)+sin(y), c("x", "y")))))
	print(yacas(expression(deriv(list(sin(x), cos(x)), x))))

	# various forms of integrate

	print(yacas(expression(integrate(sin))))
	print(yacas(expression(integrate(sin, 0, pi))))
	print(yacas(expression(integrate(sin(x), x))))
	print(yacas(expression(integrate(sin(x), x, 0, pi))))
	print(yacas(expression(integrate(sin(x), "x"))))
	print(yacas(expression(integrate(sin(x), "x", 0, pi))))

	print(yacas(expression(integrate(tan, q, pi/(12%/%3)))))
	print(yacas(expression(integrate(tan, -pi/2, pi/(12%/%3)))))
	print(yacas(expression(integrate(tan, 1/3, pi/(12%/%3)))))
	res <- yacas(expression(integrate(tan, 1/3, pi/(12%/%3))))
	print(res)
	print(Eval(res))
	
#	print(yacas(expression(Simplify(integrate(x*x,x))), v = TRUE))
#	print(yacas(expression(integrate(x*x,x)), v = TRUE))
	print(yacas(expression(Simplify(integrate(x*x,x)))))
	print(yacas(expression(integrate(x*x,x))))

	# get yacas version
	Ver()



