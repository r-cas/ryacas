\name{Sym}
\alias{Sym}

%- Also NEED an '\alias' for EACH other topic documented here.

\alias{Expr}
\alias{Exprq}
\alias{Ops.Expr}
\alias{Math.Expr}
\alias{deriv.Expr}
\alias{print.Expr}
\alias{as.character.Expr}
\alias{as.Sym}
\alias{as.Sym.Expr}
\alias{as.Sym.yacas}

\alias{as.character.Sym}
\alias{as.expression.Sym}
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
\alias{Inverse}

\alias{as.Expr.formula }
\alias{Clear.Expr }
\alias{Clear.Sym }
\alias{Conjugate.Expr }
\alias{Conjugate.Sym}
\alias{determinant.Expr }
\alias{Eval.Expr }
\alias{Expand.Expr }
\alias{Expand.Sym }
\alias{Factor.Expr}
\alias{Factor.Sym }
\alias{Factorial.Expr }
\alias{Factorial.Sym }
\alias{Integrate.Expr }
\alias{Integrate.Sym}
\alias{Inverse.Expr }
\alias{Inverse.Sym }
\alias{InverseTaylor.Sym }
\alias{Limit.Expr }
\alias{Limit.Sym}
\alias{List.Expr }
\alias{List.Sym }
\alias{N.Expr }
\alias{N.Sym }
\alias{Newton.Expr }
\alias{Newton.Sym }
\alias{Precision.Expr}
\alias{Precision.Sym }
\alias{PrettyForm.Expr }
\alias{PrettyForm.Sym }
\alias{PrettyPrinter.Expr}
\alias{PrettyPrinter.Sym }
\alias{Set.Expr }
\alias{Set.Sym }
\alias{Simplify.Expr}
\alias{Simplify.Sym}
\alias{Solve.Expr }
\alias{Solve.Sym }
\alias{Taylor.Expr }
\alias{Taylor.Sym }
\alias{TeXForm.Expr }
\alias{TeXForm.Sym }
\alias{Ver.Expr }
\alias{Ver.Sym}

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
Set, 
Simplify, 
Solve, 
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
acos(Sym("1/2"))

y <- Exprq(x)
y*y
deriv(y*y, y)
Exprq(acos(1/2))
}
}
\keyword{ symbolmath }
