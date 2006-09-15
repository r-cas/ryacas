\name{Sym}
\alias{Sym}

%- Also NEED an '\alias' for EACH other topic documented here.

\alias{as.character.Sym}
\alias{Deriv}
\alias{Integrate}
\alias{OpenMath2R}
\alias{Ops.Sym}
\alias{Ops.yacas.symbol}
\alias{print.Sym}
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

\alias{Sin}
\alias{Cos}
\alias{Tan}
\alias{ArcSin}
\alias{ArcCos}
\alias{ArcTan}
\alias{Log}
\alias{Ln}
\alias{Exp}
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
\alias{Determinant}
\alias{D.}

\title{ Sym }
\description{
  The Symbol interface to yacas.
}
\usage{
Sym(...)
}
\arguments{
  \item{\dots}{ An R character string or object that can be coerced to
a character string.}
}
\details{
A \code{"Sym"} object is a yacas character string with class \code{"Sym"}.
One can combine such objects using arithmetic and other operators to build
up more complex objects.  
See \code{vignette("Rycas-Sym")} for many examples.
}
\value{
Return a yacas \code{"Sym"} object.
}
\examples{
\dontrun{
x <- Sym("x")
x*x
Integrate(x*x, x)
Sym("12/24")
}
}
\keyword{ symbolmath }
