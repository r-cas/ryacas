#' R interface to yacas computer algebra package
#' 
#' Ryacas allows one to use the yacas computer algebra package entirely from
#' within R.  It takes an R expression, an R one line function or a yacas
#' string and returns an R expression or a variety of other formats.  It can be
#' used for symbolic mathematics, exact arithmetic, ASCII pretty printing and R
#' to TeX conversions.  The main command is \code{yacas} and \code{?yacas}
#' provides some information on installation and startup.
#' 
#' The following are sources of information on \code{"Ryacas"}: \tabular{ll}{
#' DESCRIPTION file\tab library(help = Ryacas)\cr List of demo files\tab
#' demo(package = "Ryacas")\cr Demo file\tab demo("Ryacas")\cr Demo\tab
#' demo("Ryacas-PrettyPrinter")\cr Demo\tab demo("Ryacas-Function")\cr Demo\tab
#' demo("Ryacas-Sym")\cr Demo\tab demo("Ryacas-Expr")\cr List Vignettes\tab
#' vignette(package = "Ryacas")\cr Vignette\tab vignette("Ryacas")\cr This
#' File\tab package?Ryacas\cr Help files\tab ?yacas, ?yacasTranslations,
#' ?yacmode, ?Sym\cr Help files - Windows\tab ?yacasInstall\cr News\tab
#' RShowDoc("NEWS", package = "Ryacas")\cr Acknowledgements\tab
#' RShowDoc("THANKS", package = "Ryacas")\cr Wish List\tab RShowDoc("WISHLIST",
#' package = "Ryacas")\cr Home page\tab http://code.google.com/p/ryacas/\cr }
#' 
#' @name Ryacas-package
#' @docType package
#' @useDynLib Ryacas, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom stats deriv
#' @note There is a note in the help file of the \link{yacas} command that
#' discusses a number of installation and startup issues.
#' @keywords programming
#' @examples
#' 
#' print(yacas(expression(integrate(1/x, x))))
#' print(yacas("Integrate(x)1/x"))
#' x <- Sym("x"); Integrate(1/x, x)
#' acos(Sym("1/2"))
#' 
NULL


#' Yacas translations
#'
#' Translations from R to the yacas computer algebra system.
#'
#' @name yacasTranslations
#' @aliases yacasTranslations root
#' @note The translation process occurs in several steps.  If the input to the
#' \code{yacas} function is an expression then it is translated to a valid
#' yacas character string (otherwise, it is sent to yacas unprocessed). Yacas
#' then processes the string and if \code{retclass="expression"} it is
#' translated back to an R expression (otherwise it is sent back unprocessed).
#' Currently supported translations are:
#'
#' \tabular{lll}{ CONSTANTS\tab \tab \cr R\tab \tab yacas\cr =\tab \tab
#' =====\cr pi\tab \tab Pi\cr }
#'
#'
#' \tabular{lll}{ OPERATORS\tab \tab \cr R\tab \tab yacas\cr =\tab \tab
#' =====\cr 7 \%\% 3\tab \tab Mod(7, 3)\cr 7 \%\/\% 3\tab \tab Div(7, 3)\cr }
#' 
#'
#' \tabular{lll}{ FUNCTIONS\tab \tab \cr R\tab \tab yacas\cr =\tab \tab
#' =====\cr sin(x)\tab \tab Sin(x)\cr cos(x)\tab \tab Cos(x)\cr tan(x)\tab \tab
#' Tan(x)\cr asin(x)\tab \tab ArcSin(x)\cr acos(x)\tab \tab ArcCos(x)\cr
#' atan(x)\tab \tab ArcTan(x)\cr exp(x)\tab \tab Exp(x)\cr sqrt(x)\tab \tab
#' Sqrt(x)\cr log(x)\tab \tab Ln(x)\cr choose(n, k)\tab \tab Bin(n, k)\cr
#' gamma(x)\tab \tab Gamma(x)\cr deriv(sin, x)\tab \tab Deriv(x)Sin(x)\cr
#' integrate(f, a, b)\tab \tab Integrate(x, a, b)f(x)\cr list()\tab \tab
#' List()\cr factorial(n)\tab \tab n!\cr }
#'
#' Note the Limit example in demo(Ryacas) for adding translations on the fly.
#'
#' The complete table under development.
#' @author Rob J Goedman
#' @references \url{http://yacas.sourceforge.net}
#' @keywords symbolmath
NULL
