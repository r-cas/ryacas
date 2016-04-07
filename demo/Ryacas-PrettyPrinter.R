
# ASCII pretty printing
yacas("PrettyPrinter'Set(\"PrettyForm\")")
yacas("{{a,b},{c,d}}")
yacas('(x+y)^3-(x-y)^3')
yacas('Simplify(%)')
yacas('Limit(x,0) Sin(x)/x ')
yacas('Limit(x,0) (Sin(x)-Tan(x))/(x^3)')
yacas('Limit(x,0) 1/x ')
yacas('Limit(x,0,Left) 1/x ')
yacas('Limit(x,0,Right) 1/x ')
yacas('A:={{1,2},{a,6}}')
yacas('MatrixPower(A,3)')

# set to yacas form
yacas("PrettyPrinter'Set()")
yacas("A")

# reset back to the default OMForm
yacas("PrettyPrinter'Set(\"OMForm\")")
yacas(expression(x+x))

# emit one line of TeX code
yacas("x1 := (1+x)^2 + k^3")
yacas("TeXForm(x1)", retclass = "unquote")

