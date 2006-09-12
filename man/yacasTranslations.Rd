\name{yacasTranslations}
\alias{yacasTranslations} 
\alias{root}
\title{ Yacas translations }
\description{
  Translations from R to the yacas computer algebra system.
}

\note{The translation process occurs in several
steps.  If the input to the \code{yacas} function is an
expression then it is translated to a valid yacas
character string (otherwise, it is sent to yacas unprocessed).
Yacas then processes the string and if \code{retclass="expression"}
it is translated back to an R expression (otherwise it is sent
back unprocessed).  Currently supported translations are:

\tabular{lll}{
CONSTANTS\tab \tab \cr
R\tab \tab yacas\cr
=\tab \tab =====\cr
pi\tab \tab Pi\cr
}

-----------

\tabular{lll}{
OPERATORS\tab \tab \cr
R\tab \tab yacas\cr
=\tab \tab =====\cr
7 \%\% 3\tab \tab Mod(7, 3)\cr
7 \%\/\% 3\tab \tab Div(7, 3)\cr
}

-----------

\tabular{lll}{
FUNCTIONS\tab \tab \cr
R\tab \tab yacas\cr
=\tab \tab =====\cr
sin(x)\tab \tab Sin(x)\cr
cos(x)\tab \tab Cos(x)\cr
tan(x)\tab \tab Tan(x)\cr
asin(x)\tab \tab ArcSin(x)\cr
acos(x)\tab \tab ArcCos(x)\cr
atan(x)\tab \tab ArcTan(x)\cr
exp(x)\tab \tab Exp(x)\cr
sqrt(x)\tab \tab Sqrt(x)\cr
log(x)\tab \tab Ln(x)\cr
choose(n, k)\tab \tab Bin(n, k)\cr
gamma(x)\tab \tab Gamma(x)\cr
deriv(sin, x)\tab \tab Deriv(x)Sin(x)\cr
integrate(f, a, b)\tab \tab Integrate(x, a, b)f(x)\cr
list()\tab \tab List()\cr
factorial(n)\tab \tab n!\cr
}

Note the Limit example in demo(Ryacas) for adding translations
on the fly.

The complete table under development.
}

\references{ \url{http://yacas.sourceforge.net} }
\author{ Rob J Goedman }
\keyword{ symbolmath }
