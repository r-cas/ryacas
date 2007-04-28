Expr <- function(x) structure(as.expression(x), class = c("Expr", "expression"))
Exprq <- function(x) Expr(substitute(x))

as.character.Expr <- function(x, ...) as.character(unclass(x))

Ops.Expr <- function (e1, e2) {
   L <- c(.Generic = as.name(.Generic), as.list(match.call())[-1])
   Expr(if (missing(e2)) { 
      substitute(.Generic(e1), L)
   } else {
      substitute(.Generic(e1, e2), L)
   })
}

Math.Expr <- function(x, ...) {
	idx <- match(.Generic, transtab[,1], nomatch = 0)
	fn <- if (idx > 0) transtab[idx, 3] else .Generic
	L <- c(fn = as.name(fn), as.list(match.call())[-1])
	Expr(substitute(fn(x), L))
}


deriv.Expr <- function(expr, name = Exprq(x), n = 1, ...)
	Expr(substitute(deriv(expr,name,n), as.list(match.call())[-1]))

print.Expr <- function(x, ...) print(yacas(x, ...))

Integrate.Expr <- function(f, x, a, b, ...) {
   if (missing(a) && missing(b)) { 
      Expr(substitute(integrate(f, x), as.list(match.call())[-1]))
   } else Expr(substitute(integrate(f, a, b, x), as.list(match.call())[-1]))
}

Eval.Expr <- function(x, env = parent.frame(), ...) 
	eval(yacas(x, ...)[[1]], env = env)

Simplify.Expr <- function(x, ...) 
   Expr(substitute(Simplify(x), as.list(match.call())[-1]))

Factorial.Expr <- function(x) 
   Expr(substitute(Factorial(x), as.list(match.call())[-1]))

List.Expr <- function(x, ...) 
   Expr(substitute(List(x, ...), as.list(match.call())[-1]))

N.Expr <- function(x, ...)
   Expr(substitute(N(x, ...), as.list(match.call())[-1]))

Ver.Expr <- function(x) Exprq(Version())

Clear.Expr <- function(x, ...)
   Expr(substitute(Clear(x, ...), as.list(match.call())[-1]))

Factor.Expr <- function(x, ...)
   Expr(substitute(Factor(x), as.list(match.call())[-1]))

Expand.Expr <- function(x, ...)
   Expr(substitute(Expand(x), as.list(match.call())[-1]))

Taylor.Expr <- function(f, x, a, n, ...) 
   Expr(substitute(Taylor(f, x, a, n, ...), as.list(match.call())[-1]))

PrettyForm.Expr <- function(x, ...) 
   Expr(substitute(PrettyForm(x), as.list(match.call())[-1]))

TeXForm.Expr <- function(x, ...)
   Expr(substitute(TeXForm(x), as.list(match.call())[-1]))

Precision.Expr <- function(x, ...)
   Expr(substitute(Precision(x, ...), as.list(match.call())[-1]))

Conjugate.Expr <- function(x, ...) 
   Expr(substitute(Conjugate(x, ...), as.list(match.call())[-1]))

PrettyPrinter.Expr <- function(x, ...) {
   if (missing(x)) Exprq(PrettyPrinter())
   else Expr(substitute(PrettyPrinter(x, ...), as.list(match.call())[-1]))
}

Solve.Expr <- function(x, y, ...) 
   Expr(substitute(Solve(x, ...), as.list(match.call())[-1]))

Newton.Expr <- function(x, ...)
   Expr(substitute(Newton(x, ...), as.list(match.call())[-1]))

# Set -- see Sym2.R

Limit.Expr <- function(f, x, a, ...) 
   Expr(substitute(Limit(x, ...), as.list(match.call())[-1]))

Inverse.Expr <- function(x, ...) 
   Expr(substitute(Inverse(x, ...), as.list(match.call())[-1]))

determinant.Expr <- function(x, ...)
   Expr(substitute(Determinant(x, ...), as.list(match.call())[-1]))


