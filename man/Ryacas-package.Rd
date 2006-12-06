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
DESCRIPTION file\tab library(help = Ryacas)\cr
List of demo files\tab demo(package = "Ryacas")\cr
Demo file\tab demo("Ryacas")\cr
Demo\tab demo("Ryacas-PrettyPrinter")\cr
Demo\tab demo("Ryacas-Function")\cr
Demo\tab demo("Ryacas-Sym")\cr
Demo\tab demo("Ryacas-Expr")\cr
List Vignettes\tab vignette(package = "Ryacas")\cr
Vignette\tab vignette("Ryacas")\cr
This File\tab package?Ryacas\cr
Help files\tab ?yacas, ?yacasTranslations, ?yacmode, ?Sym\cr
Help files - Windows\tab ?yacasInstall\cr
News\tab file.show(system.file("NEWS", package = "Ryacas"))\cr
Acknowledgements\tab file.show(system.file("THANKS", package = "Ryacas"))\cr
Wish List\tab file.show(system.file("WISHLIST", package = "Ryacas"))\cr
Home page\tab http://code.google.com/p/ryacas/\cr
}
}
\note{There is a note in the help file of the \link{yacas} command
that discusses a number of installation and startup issues.}

\examples{
\dontrun{
print(yacas(expression(integrate(1/x, x))))
print(yacas("Integrate(x)1/x"))
x <- Sym("x"); Integrate(1/x, x)
acos(Sym("1/2"))
}
}
\keyword{programming}
