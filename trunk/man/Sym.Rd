\name{Sym}
\alias{Sym}

%- Also NEED an '\alias' for EACH other topic documented here.

\alias{Expr}
\alias{Ops.Expr}
\alias{Math.Expr}
\alias{deriv.Expr}
\alias{print.Expr}
\alias{as.character.Expr}

\alias{as.character.Sym}
\alias{deriv.Sym}
\alias{Integrate}
\alias{OpenMath2R}
\alias{Ops.Sym}
\alias{Math.Sym}
\alias{Ops.yacas.symbol}
\alias{print.Sym}
\alias{determinant.Sym}
\alias{print.yacas}
\alias{Sym}
\alias{SymExpr}
\alias{trans}
\alias{transtab}
\alias{yacas.symbol.value}
\alias{yDeriv}
\alias{yFactorial}
\alias{yIntegrate}
\alias{yLimit}
\alias{yrewrite}
\alias{yUnlist}

\alias{Simplify}
\alias{Factorial}
\alias{List}
\alias{Ver}
\alias{Seq}
\alias{N}
\alias{Pi}
\alias{Clear}
\alias{Factor}
\alias{Expand}
\alias{Taylor}
\alias{InverseTaylor}
\alias{PrettyForm}
\alias{TeXForm}
\alias{Precision}
\alias{Conjugate}
\alias{PrettyPrinter}
\alias{Solve}
\alias{Newton}
\alias{Set}
\alias{Infinity}
\alias{I}
\alias{Limit}
\alias{Subst}
\alias{Inverse}

\title{ Sym }
\description{
  The Symbol interface to yacas.
}
\usage{
Sym(...)
Expr(x)
}
\arguments{
  \item{x}{An R expression.}
  \item{\dots}{ An R character string or object that can be coerced to
a character string.}
}
\details{
An object of class  \code{"Sym"} is internally a yacas character string.
An object of class \code{"Expr"} is internally an R expression.
One can combine such objects using the Math and Ops R operators (see
help(Math) and help(Ops) for a list).  Also there are methods for a
number of R generics: 
\code{as.character.Sym},
\code{determinant.Sym},
\code{deriv.Sym} and
\code{print.Sym}
and yacas-oriented functions:
Clear,
Conjugate, 
Determinant 
Expand, 
Factor, 
Factorial, 
I, 
Infinity,
Integrate, 
Inverse,
InverseTaylor, 
Limit, 
List, 
N, 
Newton, 
Pi, 
Precision, 
PrettyForm, 
PrettyPrinter, 
Seq, 
Set, 
Simplify, 
Solve, 
Subst, 
Taylor, 
TeXForm,
and Ver
all of which have
the same meaning as the corresponding yacas commands.
Try \code{vignette("Rycas-Sym")} for many examples.
}
\value{
\code{Sym} returns a \code{"Sym"} object and \code{Expr} returns an
\code{"Expr"} object.
}
\note{Currently the only \code{Expr} methods implemented are
as.character.Expr, deriv.Expr, Math.Expr, Ops.Expr and print.Expr.}
\examples{
\dontrun{
x <- Sym("x")
x*x
Integrate(x*x, x)
Sym("12/24")
}
}
\keyword{ symbolmath }
