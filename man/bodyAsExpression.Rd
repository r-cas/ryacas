\name{bodyAsExpression}
\alias{bodyAsExpression}
\alias{as.language}
\title{ Get body of function as an expression. }
\description{
Get body of function as an expression.
}
\usage{
bodyAsExpression(x)
}
\arguments{
  \item{x}{ An R function. }
}
\details{
  This function is similar to the R \code{body} function except
that function returns a call object whereas this one returns
an expression usable in Ryacas calculations.
}
\value{
  An expression.
}
\seealso{ \code{\link{body}} }
\examples{
\dontrun{
## try demo("Ryacas-Function") for an example.
}
}
\keyword{ symbolmath }
