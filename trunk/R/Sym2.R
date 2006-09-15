
# experimental user interface for Ryacas
# Not yet complete.

# To run, load Ryacas library and then source this code.
# Then try these examples:

# Example:
# x <- Sym("x")
# x*x+1

# Internals:
# - A Sym (i.e. symbol) object is a character string with class "Sym".
# - The ordinary R operators: +, -, etc. can operate on Sym objects
# giving new Sym objects.
# - printing a Sym object causes it to be passes to yacas and back

Sym <- function(...) {
   args <- list(...)
   value <- if (length(args) > 1) paste("(", ..., ")") else paste(args[[1]])
   class(value) <- "Sym"
   value
}

as.character.Sym <- function(x, ...) as.character(unclass(x))
Ops.Sym <- function (e1, e2) 
    if (missing(e2)) { Sym(.Generic, e1)
    } else Sym(e1, .Generic, e2)

print.Sym <- function(x, ...) print(yacas(unclass(x)))

D. <- Deriv <- function(x, y = "x", n = 1) Sym("D(", y, ",", n, ")", x)

Integrate <- function(f, x, a, b) {
   if (missing(a) && missing(b)) { Sym("Integrate(", x, ")", f)
   } else Sym("Integrate(", x, ",", a, ",", b, ")", f)
}

Eval.Sym <- function(x, env = parent.frame(), ...) 
	eval(yacas(unclass(x))[[1]], env = env)

Sin <- function(x) Sym("Sin(", x, ")")
Cos <- function(x) Sym("Cos(", x, ")")
Tan <- function(x) Sym("Tan(", x, ")")

ArcSin <- function(x) Sym("ArcSin(", x, ")")
ArcCos <- function(x) Sym("ArcCos(", x, ")")
ArcTan <- function(x) Sym("ArcTan(", x, ")")

Ln <- Log <- function(x) Sym("Ln(", x, ")")
Exp <- function(x) Sym("Exp(", x, ")")
Simplify <- function(x) Sym("Simplify(", x, ")")
Factorial <- function(x) Sym("Factorial(", x, ")")
List <- function(...) Sym("List(", paste(..., sep = ","), ")")
Seq <- function(...) do.call(List, as.list(seq(...)))
N <- function(...) Sym("N(", paste(..., sep = ","), ")")
Pi <- Sym("Pi")
Ver <- function() Sym("Version()")

Clear <- function(x) Sym("Clear(", x, ")")
Factor <- function(x) Sym("Factor(", x, ")")
Expand <- function(x) Sym("Expand(", x, ")")
Taylor <- function(f, x, a, n) Sym("Taylor(", x, ",", a, ",", n, ")", f) 
InverseTaylor <- function(f, x, a, n) 
	Sym("InverseTaylor(", x, ",", a, ",", n, ")", f) 
PrettyForm <- function(x) Sym("PrettyForm(", x, ")")
TeXForm <- function(x) Sym("TeXForm(", x, ")")
Precision <- function(x) Sym("Precision(", x, ")")
Conjugate <- function(x) Sym("Conjugate(", x, ")")
PrettyPrinter <- function(x) {
	if (missing(x)) Sym("PrettyPrinter()")
	else Sym(paste('PrettyPrinter("', x, '")', sep = ""))
}
Solve <- function(x, y) Sym("Solve(", x, ",", y, ")")
Newton <- function(...) Sym("Newton(", paste(..., sep = ","), ")")

Set <- function(x, value) 
	yacas(unclass(Sym(deparse(substitute(x)), ":=", value)))

Infinity <- Sym("Infinity")
I <- Sym("I")

Limit <- function(f, x, a) Sym("Limit(", x, ",", a, ")", f)

Subst <- function(expr, x, replacement) 
	Sym("Subst(", x, ",", replacement, ")", expr)

Inverse <- function(x) Sym("Inverse(", x, ")")
Determinant <- function(x) Sym("Determinant(", x, ")")


