\name{Eval}
\alias{Eval}
\alias{Eval.Sym}
\alias{Eval.Expr}
\alias{Eval.yacas}
\title{ Evaluate a yacas expression. }
\description{
  Evaluate a yacas expression.
}
\usage{
\method{Eval}{yacas}(x, env = parent.frame(), ...)
\method{Eval}{Sym}(x, env = parent.frame(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Object to be evaluated. }
  \item{env}{ Environment or list in which to perform evaluation. }
  \item{\dots}{ Not currently used. }
}
\examples{
\dontrun{
Eval(yacas(expression(x*x)), list(x=2))

# same
x <- 2
Eval(yacas(expression(x*x)))
}
}
\keyword{ symbolmath }%
