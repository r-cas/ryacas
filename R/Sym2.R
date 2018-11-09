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



#' Sym
#' 
#' The Symbol interface to yacas.
#' 
#' An object of class \code{"Sym"} is internally a yacas character string. An
#' object of class \code{"Expr"} is internally an R expression. One can combine
#' such objects using the Math and Ops R operators (see help(Math) and
#' help(Ops) for a list).  Also there are methods for a number of R generics:
#' \code{as.character.Sym}, \code{as.expression.Sym}, \code{determinant.Sym},
#' \code{deriv.Sym} and \code{print.Sym} and yacas-oriented functions: Clear,
#' Conjugate, Expand, Factor, Factorial, I, Identity, Infinity, Integrate,
#' Inverse, InverseTaylor, Limit, List, N, Newton, Pi, Precision, PrettyForm,
#' PrettyPrinter, Set, Simplify, Solve, Subst, Taylor, TeXForm, Ver and
#' "%Where%" all of which have the same meaning as the corresponding yacas
#' commands. Try \code{vignette("Rycas-Sym")} for many examples.
#' 
#' @aliases Sym Expr Exprq Ops.Expr Math.Expr deriv.Expr print.Expr
#' as.character.Expr as.Sym as.Sym.Expr as.Sym.yacas as.character.Sym
#' as.expression.Sym deriv.Sym Integrate OpenMath2R Ops.Sym Math.Sym
#' Ops.yacas.symbol print.Sym determinant.Sym print.yacas Sym SymExpr trans
#' transtab yacas.symbol.value yDeriv yFactorial yIntegrate yLimit yrewrite
#' yUnlist Simplify Factorial List Ver N Pi Clear Factor Expand Taylor
#' InverseTaylor PrettyForm TeXForm Precision Conjugate PrettyPrinter Solve
#' Newton Set Infinity I Limit Inverse as.Expr.formula Clear.Expr Clear.default
#' Conjugate.Expr Conjugate.default determinant.Expr Expand.Expr Expand.default
#' Factor.Expr Factor.default Factorial.Expr Factorial.default Integrate.Expr
#' Integrate.default Inverse.Expr Inverse.default InverseTaylor.default
#' Limit.Expr Limit.default List.Expr List.default N.Expr N.default Newton.Expr
#' Newton.default Precision.Expr Precision.default PrettyForm.Expr
#' PrettyForm.default PrettyPrinter.Expr PrettyPrinter.default Simplify.Expr
#' Simplify.default Solve.Expr Solve.default Taylor.Expr Taylor.default
#' TeXForm.Expr TeXForm.default Ver.Expr Ver.default Identity.default Identity
#' Subst Subst.default %Where% %Where%.default
#' @param x An R expression.
#' @param \dots An R character string or object that can be coerced to a
#' character string.
#' @return \code{Sym} returns a \code{"Sym"} object and \code{Expr} returns an
#' \code{"Expr"} object.
#' @note Currently the only \code{Expr} methods implemented are
#' as.character.Expr, deriv.Expr, Math.Expr, Ops.Expr and print.Expr.
#' @keywords symbolmath
#' @examples
#' 
#' x <- Sym("x")
#' x*x
#' Integrate(x*x, x)
#' Sym("%") %Where% list(x = 10)
#' 
#' acos(Sym("1/2"))
#' 
#' y <- Exprq(x)
#' y*y
#' deriv(y*y, y)
#' Exprq(acos(1/2))
#'
#' @export
Sym <- function(...) {
   args <- list(...)
   value <- if (length(args) > 1) paste("(", ..., ")") else paste(args[[1]])
   class(value) <- c("Sym", "character")
   value
}

#' @export
as.character.Sym <- function(x, ...) as.character(unclass(x))

#' @export
as.expression.Sym <- function(x, ...) yacas(x, ...)[[1]]

#' @export
as.Sym <- function(x, ...) UseMethod("as.Sym")

#' @export
as.Sym.yacas <- function(x, ...) Sym(format(yparse(x[[1]])))

#' @export
as.Sym.Expr <- function(x, ...) Sym(format(yparse(x)))

#' @export
Ops.Sym <- function (e1, e2) 
    if (missing(e2)) { Sym(.Generic, e1)
    } else Sym(e1, .Generic, e2)

#' @export
Math.Sym <- function(x, ...) {
	idx <- match(.Generic, transtab[,1], nomatch = 0)
	fn <- if (idx > 0) transtab[idx, 3] else .Generic
	Sym(fn, "(", x, ")")
}

#' @export
print.Sym <- function(x, ...) print(yacas(unclass(x), ...))

#' @export
deriv.Sym <- function(expr, name = "x", n = 1, ...) 
	Sym("D(", name, ",", n, ")", expr)

#' @export
Integrate <- function(f, ...) UseMethod("Integrate")

#' @export
Integrate.default <- function(f, x, a, b, ...) {
   if (missing(a) && missing(b)) { Sym("Integrate(", x, ")", f)
   } else Sym("Integrate(", x, ",", a, ",", b, ")", f)
}

#' @export
Eval.Sym <- function(x, env = parent.frame(), ...) 
	eval(yacas(unclass(x))[[1]], envir = env)

#' @export
Simplify <- function(x, ...) UseMethod("Simplify")

#' @export
Simplify.default <- function(x, ...) Sym("Simplify(", x, ")")

#' @export
Factorial <- function(x) UseMethod("Factorial")

#' @export
Factorial.default <- function(x) Sym("Factorial(", x, ")")

#' @export
List <- function(x, ...) UseMethod("List")

#' @export
List.default <- function(x, ...) Sym("List(", paste(x, ..., sep = ","), ")")

#' @export
N <- function(x, ...) UseMethod("N")

#' @export
N.default <- function(x, ...) Sym("N(", paste(x, ..., sep = ","), ")")

#' @export
Pi <- Sym("Pi")

#' @export
Ver <- function(x) UseMethod("Ver")

#' @export
Ver.default <- function(x) Sym("Version()")

#' @export
Clear <- function(x, ...) UseMethod("Clear")

#' @export
Clear.default <- function(x, ...) Sym("Clear(", x, ")")

#' @export
Factor <- function(x) UseMethod("Factor")

#' @export
Factor.default <- function(x) Sym("Factor(", x, ")")

#' @export
Expand <- function(x, ...) UseMethod("Expand")

#' @export
Expand.default <- function(x, ...) Sym("Expand(", x, ")")

#' @export
Taylor <- function(f, ...) UseMethod("Taylor")

#' @export
Taylor.default <- function(f, x, a, n, ...) 
	Sym("Taylor(", x, ",", a, ",", n, ")", f) 

#' @export
InverseTaylor <- function(x, ...) UseMethod("Taylor")

#' @export
InverseTaylor.default <- function(f, x, a, n, ...) 
	Sym("InverseTaylor(", x, ",", a, ",", n, ")", f) 

#' @export
PrettyForm <- function(x, ...) UseMethod("PrettyForm")

#' @export
PrettyForm.default <- function(x, ...) Sym("PrettyForm(", x, ")")

#' @export
TeXForm <- function(x, ...) UseMethod("TeXForm")
# @export
#TeXForm.default <- function(x, ...) Sym("TeXForm(", x, ")")
#' @export
TeXForm.default <- function(x, ...) as.character(as.expression(Sym("TeXForm(", x, ")")))

#' @export
Precision <- function(x, ...) UseMethod("Precision")

#' @export
Precision.default <- function(x, ...) Sym("Precision(", x, ")")

#' @export
Conjugate <- function(x, ...) UseMethod("Conjugate")

#' @export
Conjugate.default <- function(x, ...) Sym("Conjugate(", x, ")")

#' @export
PrettyPrinter <- function(x, ...) UseMethod("PrettyPrinter")

#' @export
PrettyPrinter.default <- function(x, ...) {
	if (missing(x)) Sym("PrettyPrinter()")
	else Sym(paste('PrettyPrinter("', x, '")', sep = ""))
}

#' @export
Solve <- function(x, ...) UseMethod("Solve")

#' @export
Solve.default <- function(x, y, ...) Sym("Solve(", x, ",", y, ")")

#' @export
Newton <- function(x, ...) UseMethod("Newton")

#' @export
Newton.default <- function(x, ...) Sym("Newton(", paste(x, ..., sep = ","), ")")

#' @export
Set <- function(x, value) {
	if (inherits(value, "expression")) 
		yacas(substitute(Set(x, value, as.list(match.call())[-1])))
	else
		yacas(unclass(Sym(deparse(substitute(x)), ":=", value)))
}

#' @export
Infinity <- Sym("Infinity")

#' @export
I <- Sym("I")

#' @export
Limit <- function(f, ...) UseMethod("Limit")

#' @export
Limit.default <- function(f, x, a, ...) Sym("Limit(", x, ",", a, ")", f)

#' @export
Subst <- function(expr, ...) UseMethod("Subst")

#' @export
Subst.default <- function(expr, x, replacement, ...) 
	Sym("Subst(", x, ",", replacement, ")", expr)

#' @export
Inverse <- function(x, ...) UseMethod("Inverse")

#' @export
Inverse.default <- function(x, ...) Sym("Inverse(", x, ")")

#' @export
determinant.Sym <- function(x, ...) Sym("Determinant(", x, ")")

#' @export
Identity <- function(x) UseMethod("Identity")

#' @export
Identity.default <- function(x) Sym("Identity(", x, ")")

#' @export
"%Where%" <- function(x, y) UseMethod("%Where%")

#' @export
"%Where%.default" <- function(x, y) {
	Sym(x, "Where", paste("{", names(y)[[1]], "==", Sym(y[[1]]), "}"))
}
