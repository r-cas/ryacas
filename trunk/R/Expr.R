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

Math.Expr <- function(x) {
	idx <- match(.Generic, transtab[,1], nomatch = 0)
	fn <- if (idx > 0) transtab[idx, 3] else .Generic
	L <- c(fn = as.name(fn), as.list(match.call())[-1])
	Expr(substitute(fn(x), L))
}


deriv.Expr <- function(expr, name = Exprq(x), n = 1, ...)
	Expr(substitute(deriv(expr,name,n), as.list(match.call())[-1]))

print.Expr <- function(x, ...) print(yacas(x, ...))

# test
# x <- Expr(expression(x))
# x*x
# deriv(x*x + cos(x), x)

