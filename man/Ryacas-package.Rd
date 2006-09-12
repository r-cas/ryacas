\name{Ryacas-package}
\alias{Ryacas-package}
\docType{package}
\title{ R interface to yacas computer algebra package }
\description{
Ryacas allows one to use the yacas computer algebra
package entirely from within R.  It takes an R expression,
an R one line function
or a yacas string and returns an R expression or a variety
of other formats.  It can be used for symbolic mathematics,
exact arithmetic, ASCII pretty printing and R to TeX
conversions.  The main command is \code{yacas} 
and \code{?yacas} provides some information on installation
and startup.  
}
\details{
The following are sources of information on \code{"Ryacas"}:
\tabular{ll}{
List of demo files\tab demo(package = "Ryacas")\cr
Invoking a demo file\tab demo("Ryacas")\cr
Second demo\tab demo("Ryacas-PrettyPrinter")\cr
Third demo\tab demo("Ryacas-Function")\cr
Vignette\tab vignette("Ryacas")\cr
This File\tab package?Ryacas\cr
Help files\tab ?yacas, ?yacasTranslations, ?yacmode\cr
News\tab file.show(system.file("NEWS", package = "Ryacas"))\cr
Acknowledgements\tab file.show(system.file("THANKS", package = "Ryacas"))\cr
Wish List\tab file.show(system.file("WISHLIST", package = "Ryacas"))\cr
}
}

\examples{
\dontrun{
print(yacas(expression(integrate(x, 1/x))))
print(yacas("Integrate(x)1/x"))
}
}
\keyword{programming}
