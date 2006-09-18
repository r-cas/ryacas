
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

Math.Sym <- function(x) {
	idx <- match(.Generic, transtab[,1], nomatch = 0)
	fn <- if (idx > 0) transtab[idx, 3] else .Generic
	Sym(fn, "(", x, ")")
}

print.Sym <- function(x, ...) print(yacas(unclass(x)))

deriv.Sym <- function(expr, name = "x", n = 1, ...) 
	Sym("D(", name, ",", n, ")", expr)

Integrate <- function(f, x, a, b) {
   if (missing(a) && missing(b)) { Sym("Integrate(", x, ")", f)
   } else Sym("Integrate(", x, ",", a, ",", b, ")", f)
}

Eval.Sym <- function(x, env = parent.frame(), ...) 
	eval(yacas(unclass(x))[[1]], env = env)

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
determinant.Sym <- function(x, ...) Sym("Determinant(", x, ")")


