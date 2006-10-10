
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
   class(value) <- c("Sym", "character")
   value
}

as.character.Sym <- function(x, ...) as.character(unclass(x))
as.expression.Sym <- function(x, ...) yacas(x, ...)[[1]]

as.Sym <- function(x, ...) UseMethod("as.Sym")
as.Sym.yacas <- function(x, ...) Sym(format(yparse(x[[1]])))
as.Sym.Expr <- function(x, ...) Sym(format(yparse(x)))

Ops.Sym <- function (e1, e2) 
    if (missing(e2)) { Sym(.Generic, e1)
    } else Sym(e1, .Generic, e2)

Math.Sym <- function(x) {
	idx <- match(.Generic, transtab[,1], nomatch = 0)
	fn <- if (idx > 0) transtab[idx, 3] else .Generic
	Sym(fn, "(", x, ")")
}

print.Sym <- function(x, ...) print(yacas(unclass(x), ...))

deriv.Sym <- function(expr, name = "x", n = 1, ...) 
	Sym("D(", name, ",", n, ")", expr)

Integrate <- function(f, ...) UseMethod("Integrate")
Integrate.Sym <- function(f, x, a, b, ...) {
   if (missing(a) && missing(b)) { Sym("Integrate(", x, ")", f)
   } else Sym("Integrate(", x, ",", a, ",", b, ")", f)
}

Eval <- function(x, ...) UseMethod("Eval")
Eval.Sym <- function(x, env = parent.frame(), ...) 
	eval(yacas(unclass(x))[[1]], env = env)

Simplify <- function(x, ...) UseMethod("Simplify")
Simplify.Sym <- function(x, ...) Sym("Simplify(", x, ")")

Factorial <- function(x) UseMethod("Factorial")
Factorial.Sym <- function(x) Sym("Factorial(", x, ")")

List <- function(x, ...) UseMethod("List")
List.Sym <- function(x, ...) Sym("List(", paste(x, ..., sep = ","), ")")

N <- function(x, ...) UseMethod("N")
N.Sym <- function(x, ...) Sym("N(", paste(x, ..., sep = ","), ")")

Pi <- Sym("Pi")

Ver.Sym <- function() Sym("Version()")

Clear <- function(x, ...) UseMethod("Clear")
Clear.Sym <- function(x, ...) Sym("Clear(", x, ")")

Factor <- function(x) UseMethod("Factor")
Factor.Sym <- function(x) Sym("Factor(", x, ")")

Expand <- function(x, ...) UseMethod("Expand")
Expand.Sym <- function(x, ...) Sym("Expand(", x, ")")

Taylor <- function(f, ...) UseMethod("Taylor")
Taylor.Sym <- function(f, x, a, n, ...) 
	Sym("Taylor(", x, ",", a, ",", n, ")", f) 

InverseTaylor <- function(x, ...) UseMethod("Taylor")
InverseTaylor.Sym <- function(f, x, a, n, ...) 
	Sym("InverseTaylor(", x, ",", a, ",", n, ")", f) 

PrettyForm <- function(x, ...) UseMethod("PrettyForm")
PrettyForm.Sym <- function(x, ...) Sym("PrettyForm(", x, ")")

TeXForm <- function(x, ...) UseMethod("TeXForm")
TeXForm.Sym <- function(x, ...) Sym("TeXForm(", x, ")")

Precision <- function(x, ...) UseMethod("Precision")
Precision.Sym <- function(x, ...) Sym("Precision(", x, ")")

Conjugate <- function(x, ...) UseMethod("Conjugate")
Conjugate.Sym <- function(x, ...) Sym("Conjugate(", x, ")")

PrettyPrinter <- function(x, ...) UseMethod("PrettyPrinter")
PrettyPrinter.Sym <- function(x, ...) {
	if (missing(x)) Sym("PrettyPrinter()")
	else Sym(paste('PrettyPrinter("', x, '")', sep = ""))
}

Solve <- function(x, ...) UseMethod("Solve")
Solve.Sym <- function(x, y, ...) Sym("Solve(", x, ",", y, ")")

Newton <- function(x, ...) UseMethod("Newton")
Newton.Sym <- function(x, ...) Sym("Newton(", paste(x, ..., sep = ","), ")")

Set <- function(x, ..., value) UseMethod("Set")
Set.Sym <- function(x, ..., value) 
	yacas(unclass(Sym(deparse(substitute(x)), ":=", value)))

Infinity <- Sym("Infinity")
I <- Sym("I")

Limit <- function(f, ...) UseMethod("Limit")
Limit.Sym <- function(f, x, a, ...) Sym("Limit(", x, ",", a, ")", f)

# Subst <- function(expr, ...) UseMethod("Subst")
# Subst.Sym <- function(expr, x, replacement, ...) 
#    Sym("Subst(", x, ",", replacement, ")", expr)

Inverse <- function(x, ...) UseMethod("Inverse")
Inverse.Sym <- function(x, ...) Sym("Inverse(", x, ")")

determinant.Sym <- function(x, ...) Sym("Determinant(", x, ")")


