\name{yacas}
\alias{isConnection} 
\alias{yacas.character} 
\alias{yacas.expression} 
\alias{yacas.function} 
\alias{yacasInvokeString}
\alias{yacas}
\alias{as.expression.yacas}
\alias{as.character.yacas}
\alias{addSemi}
\alias{haveYacas}
\alias{yacasStart}
\alias{yacasStop}
\alias{ynext}
\alias{ysub}
\alias{yparse}
\alias{yAssignFunction}
\title{ yacas interface }
\description{
  Interface to the yacas computer algebra system.
}
\usage{
\method{yacas}{character}(x, verbose = FALSE, method = c("socket", "system"), 
retclass = c("expression", "character", "unquote"), addSemi = TRUE, ...)
\method{yacas}{expression}(x, verbose = FALSE, method = c("socket", "system"), retclass = c("expression", "character", "unquote"), ...)
\method{yacas}{function}(x, verbose = FALSE, method = c("socket", "system"), ...)
}
\arguments{
  \item{x}{ A yacas character string or an R 
  expression without terminating semicolon to be processed by yacas. }
  \item{verbose}{ A logical value indicating verbosity of output. }
  \item{method}{ method used to communicate with yacas.  If \code{"socket"} is
   specified then the same yacas session is used on a sequence of calls.
   If \code{"system"} is specified then a new instance of yacas is used just
   for the period of that call.}
  \item{addSemi}{If \code{TRUE} a semicolon is added to the
   character string sent to yacas.  This can be set to \code{FALSE} if
   its known that the character string already has a trailing 
   semicolon.  It is ignored if \code{retclass="expression"}.}
  \item{retclass}{The class of the first component of the yacas structure.
   It defaults to \code{"expression"" but may be specified as  
   \code{"character"} or \code{"unquote"}.  \code{"unquote"} is the 
   same as \code{"character"} except that if the character string
   returned would have otherwise had quotes in the first and 
   and last positions then they are stripped.}
  

If }
  \item{...}{Other arguments.  Not currently used.}
}
\details{
  The user supplies an R expression, an R function with a single line body 
  or a yacas input string.
}
\value{
  An R object of class \code{"yacas"} is returned.  If 
  \code{PrettyPrinter("OMForm")} is in effect, which it is by default,
  then the first component is an R expression and the \code{OMForm}
  component contains OpenMath XML code.  In other cases the first
  component is NULL and the \code{YacasForm} or \code{PrettyForm}
  components have display information.
}
\references{ \url{http://yacas.sourceforge.net} }

\note{ Installation. The Windows version of this software includes a
binary version of yacas but on other platforms the user must
install yacas 1.0.62 as well as installing this package.  Normally
the default locations of yacas, its initialization file and
the scripts file are sufficient but, if necessary, they can
be overridden via the environment variables: \code{YACAS_HOME},
\code{YACAS_INIT} and \code{YACAS_SCRIPTS}.  

yacmode. There is also
a utility yacmode which is called without arguments and
just turns R into a terminal into yacas until one quits out of
it (and back to R) by entering stop, end, quit, exit or e.

Startup. \code{yacas} starts up when \code{yacasStart()} is called or
the first time \code{yacas} is called.  yacas is shut down
when \code{yacasStop()} is called or when the package is
detached using the \code{detach()} R command.  On Windows,
when yacas is shut down, the yacas process is terminated
on Windows XP Pro but not on other versions of Windows.
In those cases there will
be a dangling process that the user must terminate 
manually.  

Translation.  The translation process occurs in several
steps.  If the input to the \code{yacas} function is an
expression then it is translated to a valid yacas
character string (otherwise, it is sent to yacas unprocessed).
Yacas then processes the string and if \code{retclass="expression"}
it is translated back to an R expression (otherwise it is sent
back unprocessed).  Examples of translations are:

\tabular{ll}{
R\tab yacas\cr
sin(x)\tab Sin(x)\cr
deriv(sin, x)\tab Deriv(x)Sin(x)\cr
log(x)\tab Ln(x)\cr
}
}

\keyword{ symbolmath }

\examples{
\dontrun{
yacas(expression(Factor(x^2-1)))
exp1 <- expression(x^2 + 2 * x^2)
exp2 <- expression(2 * exp0)
exp3 <- expression(6 * pi * x)
exp4 <- expression((exp1 * (1 - sin(exp3))) / exp2)
print(yacas(exp4))

print(yacas("Version()") # yacas version

# see demo("Ryacas-Function")


}
}


