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

\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Sym(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (...) 
{
    args <- list(...)
    value <- if (length(args) > 1) 
        paste("(", ..., ")")
    else args[[1]]
    class(value) <- "Sym"
    value
  }
}
\keyword{ symbolmath }% at least one, from doc/KEYWORDS
