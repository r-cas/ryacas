#' @export
Expr <- function(x) structure(as.expression(x), class = c("Expr", "expression"))

#' @export
Exprq <- function(x) Expr(substitute(x))

#' @export
as.character.Expr <- function(x, ...) as.character(unclass(x))

#' @export
Ops.Expr <- function (e1, e2) {
   L <- c(.Generic = as.name(.Generic), as.list(match.call())[-1])
   Expr(if (missing(e2)) { 
      substitute(.Generic(e1), L)
   } else {
      substitute(.Generic(e1, e2), L)
   })
}

#' @export
Math.Expr <- function(x, ...) {
	idx <- match(.Generic, transtab[,1], nomatch = 0)
	fn <- if (idx > 0) transtab[idx, 3] else .Generic
	L <- c(fn = as.name(fn), as.list(match.call())[-1])
	Expr(substitute(fn(x), L))
}

#' @export
deriv.Expr <- function(expr, name = Expr(as.name("x")), n = 1, ...)
	Expr(substitute(deriv(expr,name,n), as.list(match.call())[-1]))

#' @export
print.Expr <- function(x, ...) print(yacas(x, ...))

#' @export
Integrate.Expr <- function(f, x, a, b, ...) {
   if (missing(a) && missing(b)) { 
      Expr(substitute(integrate(f, x), as.list(match.call())[-1]))
   } else Expr(substitute(integrate(f, a, b, x), as.list(match.call())[-1]))
}

#' @export
Eval.Expr <- function(x, env = parent.frame(), ...) 
	eval(yacas(x, ...)[[1]], envir = env)

#' @export
Simplify.Expr <- function(x, ...) 
   Expr(substitute(Simplify(x), as.list(match.call())[-1]))

#' @export
Factorial.Expr <- function(x) 
   Expr(substitute(Factorial(x), as.list(match.call())[-1]))

#' @export
List.Expr <- function(x, ...) 
   Expr(substitute(List(x, ...), as.list(match.call())[-1]))

#' @export
N.Expr <- function(x, ...)
   Expr(substitute(N(x, ...), as.list(match.call())[-1]))

#' @export
Ver.Expr <- function(x) Exprq(Ver())

#' @export
Clear.Expr <- function(x, ...)
   Expr(substitute(Clear(x, ...), as.list(match.call())[-1]))

#' @export
Factor.Expr <- function(x, ...)
   Expr(substitute(Factor(x), as.list(match.call())[-1]))

#' @export
Expand.Expr <- function(x, ...)
   Expr(substitute(Expand(x), as.list(match.call())[-1]))

#' @export
Taylor.Expr <- function(f, x, a, n, ...) 
   Expr(substitute(Taylor(f, x, a, n, ...), as.list(match.call())[-1]))

#' @export
PrettyForm.Expr <- function(x, ...) 
   Expr(substitute(PrettyForm(x), as.list(match.call())[-1]))

# @export
#TeXForm.Expr <- function(x, ...)
#   Expr(substitute(TeXForm(x), as.list(match.call())[-1]))
#TeXForm.Expr <- function(x, ...)
#   Expr(substitute(TeXForm(x), as.list(match.call())[-1]))

#' @export
Precision.Expr <- function(x, ...)
   Expr(substitute(Precision(x, ...), as.list(match.call())[-1]))

#' @export
Conjugate.Expr <- function(x, ...) 
   Expr(substitute(Conjugate(x, ...), as.list(match.call())[-1]))

#' @export
PrettyPrinter.Expr <- function(x, ...) {
   if (missing(x)) Exprq(PrettyPrinter())
   else Expr(substitute(PrettyPrinter(x, ...), as.list(match.call())[-1]))
}

#' @export
Solve.Expr <- function(x, y, ...) 
   Expr(substitute(Solve(x, ...), as.list(match.call())[-1]))

#' @export
Newton.Expr <- function(x, ...)
   Expr(substitute(Newton(x, ...), as.list(match.call())[-1]))

# Set -- see Sym2.R

#' @export
Limit.Expr <- function(f, x, a, ...) 
   Expr(substitute(Limit(x, ...), as.list(match.call())[-1]))

#' @export
Inverse.Expr <- function(x, ...) 
   Expr(substitute(Inverse(x, ...), as.list(match.call())[-1]))

#' @export
determinant.Expr <- function(x, ...)
   Expr(substitute(Determinant(x, ...), as.list(match.call())[-1]))
