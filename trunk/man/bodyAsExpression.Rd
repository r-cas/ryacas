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

# construct an R function for the Burr probability density
# function (PDF) given the Burr cumulative distribution function (CDF)
BurrCDF <- function(x, c = 1, k = 1) 1-(1+x^c)^-k

# transfer CDF to yacas
yacas(BurrCDF)

# create a template for the PDF from the CDF
BurrPDF <- BurrCDF

# differentiate CDF and place resulting expression in body
body(BurrPDF) <- yacas(expression(deriv(BurrCDF(x,c,k))))[[1]]

# test out PDF
BurrPDF(1)

}

}
\keyword{ symbolmath }
