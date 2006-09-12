
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
   value <- if (length(args) > 1) paste("(", ..., ")") else args[[1]]
   class(value) <- "Sym"
   value
}

as.character.Sym <- function(obj) as.character(unclass(obj))
Ops.Sym <- function (e1, e2) 
    if (missing(e2)) { Sym(.Generic, e1)
    } else Sym(e1, .Generic, e2)

print.Sym <- function(x, ...) print(yacas(unclass(x)))

Deriv <- function(x, y) Sym("Deriv(", y, ")", x)
Integrate <- function(f, a, b) {
   if (missing(a) && missing(b)) { Sym("Integrate(x)", f, "(x)")
   } else Sym("Integrate(x,", a, ",", b, ")", f, "(x)")
}

