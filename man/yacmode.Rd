\name{yacmode}
\alias{yacmode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ yacmode interface }
\description{
  Interactive interface to the yacas
}
\usage{
yacmode()
}
%- maybe also 'usage' for other objects documented here.
\details{
  The user types valid yacas input and presses return. Type
  'quit' to return to R prompt.
}
\note{ Note that command recall will recall previous R commands,
not previous yacas input. Yacas is given a limited amount of time
to complete, otherwise '[1] CommandLine(1) : User interrupted calculation'
is returned. E.g. Taylor(x,0,5) 1/(1+x) will work, but 
Taylor(x,0,12) 1/(1+x) is likely to take too long.
}
\value{
  Output of yacas is returned.
}
\references{ \url{http://yacas.sourceforge.net} }

\examples{
\dontrun{
yacmode()
 (x+y)^3-(x-y)^3
 Simplify(%)
 q
}
}

\keyword{ symbolmath }
