
# this file is currently not used

yacas.symbol.value <- function(x, verbose, method, ...) {
   x <- list(value = x)
   class(x) <- "yacas.symbol"
   x
}
   
Ops.yacas.symbol <- function (e1, e2) 
{
    e <- if (missing(e2)) yacas.symbol.value(c(.Generic, e1$value))
    else if (any(nchar(.Method) == 0)) NextMethod(.Generic)
    else yacas.symbol.value(c(e1$value, .Generic, e2$value))
}


SymExpr <- function(op, operands) {
   x <- list(op = op, operands = operands)
   class(x) <- "SymExpr"
   x
}

