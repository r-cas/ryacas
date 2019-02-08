#' yacas interface
#' 
#' Interface to the yacas computer algebra system.
#' 
#' The user supplies an R expression, an R function name corresponding to a
#' function with a single line body, a formula or a yacas input string.  In the
#' case of a formula it is regarded as an expression represented by the right
#' hand side of the formula while the left hand side, if any, is ignored.
#' 
#' Note the silent version [syacas()].
#' 
#' @aliases yacas.character yacas.expression yacas.function yacas.formula
#' yacas.yacas yacas as.expression.yacas as.character.yacas addSemi ynext
#' ySequence ysub yparse yAssignFunction
#' @param x A yacas character string or an R expression without terminating
#' semicolon to be processed by yacas.
#' @param verbose A logical value indicating verbosity of output or
#' \code{"input"} to only show input to yacas but not output from yacas or
#' \code{"output"} to only show output from yacas but not input to yacas.
#' @param method method used to communicate with yacas.  If \code{"socket"} is
#' specified then the same yacas session is used on a sequence of calls.  If
#' \code{"system"} is specified then a new instance of yacas is used just for
#' the period of that call. \code{"system"} does not require that the system be
#' configured to support telnet/sockets and so may be useful in some instances.
#' If no value is specified the default is taken from
#' \code{getOption("yacas.method")} and if that is not specified "socket" is
#' used.  \code{"socket"} and \code{"system"} may be abbreviated.
#' @param addSemi If \code{TRUE} a semicolon is added to the character string
#' sent to yacas.  This can be set to \code{FALSE} if its known that the
#' character string already has a trailing semicolon.  It is ignored if
#' \code{retclass="expression"}.
#' @param retclass The class of the first component of the yacas structure.  It
#' defaults to \code{"expression"} but may be specified as \code{"character"}
#' or \code{"unquote"}.  \code{"unquote"} is the same as \code{"character"}
#' except that if the character string returned would have otherwise had quotes
#' in the first and and last positions then they are stripped.
#' @param \dots Additional arguments ultimately passed down to
#' \code{yacas.character}.
#' @return An R object of class \code{"yacas"} is returned.  If
#' \code{PrettyPrinter("OMForm")} is in effect, which it is by default, then
#' the first component is an R expression and the \code{OMForm} component
#' contains OpenMath XML code.  In other cases the first component is NULL and
#' the \code{YacasForm} or \code{PrettyForm} components have display
#' information.
#' @usage 
#' yacas(x, ...)
#' \method{yacas}{character}(x, verbose = FALSE, method, 
#'     retclass = c("expression", "character", "unquote"), 
#'     addSemi = TRUE, ...)
#' @note Windows Installation.  On Windows one can install \code{Ryacas} by
#' issuing the commands:
#' 
#' \tabular{l}{ \code{install.packages("Ryacas", dep = TRUE)}\cr
#' \code{library(Ryacas)}\cr \code{yacasInstall()}\cr }
#' 
#' or by using the \code{Packages | Install package(s)} menu in place of the
#' first command.  The second command downloads \code{scripts.dat} and
#' \code{yacas.exe} from the internet and installs them into
#' \code{R_HOME/library/Ryacas/yacdir} where \code{R_HOME} is the location of
#' your \code{R} installation.
#' 
#' Normally the default locations of yacas, its initialization file and the
#' scripts file are sufficient but, if necessary, they can be overridden via
#' the environment variables: \code{YACAS_HOME}, \code{YACAS_INIT} and
#' \code{YACAS_SCRIPTS}. The \code{YACAS_INVOKE_STRING} environment variable
#' discussed in the next section overrides all three of these.
#' 
#' All OS Installation.  The \code{YACAS_INVOKE_STRING} environment variable
#' can be used to override the invocation string for yacas.  Normally it is not
#' used.  If it does need to be used then a typical use might be:
#' 
#' \tabular{l}{ \code{library(Ryacas)}\cr \code{# only need to do the file.copy
#' command once}\cr \code{file.copy(system.file("yacdir/R.ys", package =
#' "Ryacas"), "~/.yacsrc")}\cr \code{# this needs to be done once per
#' session}\cr \code{Sys.setenv(YACAS_INVOKE_STRING = "yacas -pc --server
#' 9734")}\cr \code{demo(Ryacas) # test it out}\cr }
#' 
#' yacmode. There is also a utility yacmode which is called without arguments
#' and just turns R into a terminal into yacas until one quits out of it (and
#' back to R) by entering stop, end, quit, exit or e.
#' 
#' Startup. \code{yacas} starts up when \code{yacasStart()} is called or the
#' first time \code{yacas} is called.  yacas is shut down when
#' \code{yacasStop()} is called or when the package is detached using the
#' \code{detach()} R command.  On Windows, when yacas is shut down, the yacas
#' process is terminated on Windows XP Pro but not on other versions of
#' Windows. In those cases there will be a dangling process that the user must
#' terminate manually.
#' 
#' Translation.  The translation process occurs in several steps.  If the input
#' to the \code{yacas} function is an expression then it is translated to a
#' valid yacas character string (otherwise, it is sent to yacas unprocessed).
#' Yacas then processes the string and if \code{retclass="expression"} it is
#' translated back to an R expression (otherwise it is sent back unprocessed).
#' Examples of translations are:
#' 
#' \tabular{ll}{ R\tab yacas\cr sin(x)\tab Sin(x)\cr deriv(sin, x)\tab
#' Deriv(x)Sin(x)\cr log(x)\tab Ln(x)\cr }
#' @references \url{http://yacas.sourceforge.net}
#' @keywords symbolmath
#' @examples
#' 
#' yacas(expression(Factor(x^2-1)))
#' exp1 <- expression(x^2 + 2 * x^2)
#' exp2 <- expression(2 * exp0)
#' exp3 <- expression(6 * pi * x)
#' exp4 <- expression((exp1 * (1 - sin(exp3))) / exp2)
#' print(yacas(exp4))
#' 
#' print(yacas("Version()")) # yacas version
#' 
#' # see demo("Ryacas-Function")
#' 
#' @export
yacas <- function(x, ...)
  UseMethod("yacas")

#' @importFrom xml2 xml_attr xml_find_all read_xml
#' @export
yacas.character <- function(x, verbose = FALSE, method, retclass = c("expression", "character", "unquote"), addSemi = TRUE, ...) {

    addSemiFn <- function(x) {
      x <- sub(";[[:blank:]]*$", "", x)
      x <- paste(x, ";", collapse='', sep='')
      return(x)
    }

    retclass <- match.arg(retclass)

    if (retclass == "expression")
    	x <- paste("Eval(",x,");")
    else
		if (addSemi)
            x <- addSemiFn(x)

    yacas.res <- yacas_evaluate(x)

    # TeXForm
    if (grepl('^<OMOBJ>.*<OMSTR>', yacas.res[1])) {
      text <- yacas.res[1]
      text <- gsub("&amp;", "&", text, fixed = TRUE)
      text <- gsub("&", "&amp;", text, fixed = TRUE)
      #text <- OpenMath2R(yacas.res[1])
      text <- OpenMath2R(text)
      #text <- gsub("\\", "\\\\", text, fixed = TRUE)
      text <- sub("^['\"](.*)['\"]", "\\1", text)
      text <- gsub("[ ]+\\$$", "$", text) # remove space before ending math-mode dollar
      text <- gsub("(^\\$)|(\\$$)", "", text) # remove math mode
      #result <- list(text = text, TeXForm = yacas.res[1])
      return(text)
      
      #class(result) <- "yacas"
      #return(result)
    } else if (grepl('^<OMOBJ>', yacas.res[1])) {
      # Matrix/vector:
      # 
      # Vector: List without nested lists
      # Matrix: List of Lists, but no futher nesting
      #
      # Cannot be based on x, as this must work for e.g. Eval
      # as well. So need to be based on XML.
      # 
      # List in yacas is "<OMS cd=\"list1\" name=\"list\"/>"
      
      try_linalg <- RYACAS_OPTIONS("module_matvec_enabled")

      LinAlgType <- NULL
      LinAlgForm <- NULL
      LinAlgDim <- NULL
      
      if (try_linalg) {
        list_depth <- get_xml_list_depth(yacas.res[1])
        #print(list_depth)
        
        #if (grepl('^<OMOBJ>.*<OMS cd=\"list1\" name=\"list\"/>.*<OMS cd=\"list1\" name=\"list\"/>', yacas.res[1])) {
        if (is.finite(list_depth) && list_depth == 2L) {
          # Matrix
          LinAlgType <- "Matrix"
          LinAlgForm <- OpenMath2RMatrix(yacas.res[1])
          LinAlgDim <- dim(LinAlgForm)
          #} else if (grepl('^<OMOBJ>.*<OMS cd=\"list1\" name=\"list\"/>', yacas.res[1])) {
        } else if (is.finite(list_depth) && list_depth == 1L) {
          # Vector; important that this comes after Matrix above
          LinAlgType <- "Vector"
          LinAlgForm <- OpenMath2RVector(yacas.res[1])
          LinAlgDim <- dim(LinAlgForm)
        } 
      }
      
      # Default / other types
      text <- OpenMath2R(yacas.res[1])
      
      if (retclass == "expression") {
        #text <- parse(text = text, srcfile = NULL)
        text <- gsub("\\", "\\\\", text, fixed = TRUE)
        text <- parse(text = text, srcfile = NULL)
      } else if (retclass == "unquote") {
        text <- sub("^['\"](.*)['\"]", "\\1", text)
      }
      
      if (!is.null(LinAlgForm)) {
        result <- list(text = text, 
                       LinAlgType = LinAlgType, 
                       LinAlgForm = LinAlgForm,
                       LinAlgDim = LinAlgDim)
      } else {
        result <- list(text = text, OMForm = yacas.res[1])
      }
    } else if (nchar(yacas.res[1]) > 0) {
        result <- list(NULL, PrettyForm = sub('<OMOBJ>.*</OMOBJ>(\r?\n)', '', yacas.res[1]))
    } else {
        result <- list(NULL, YacasForm = yacas.res[2])
    }

    class(result) <- "yacas"
    return(result)
}

# test
# yacas("Pi+Sin(x)", retclass = "character", OM=TRUE)


#' @export
as.language <- function(x) parse(text=paste(deparse(x)))[[1]]


#' Get body of function as an expression.
#' 
#' Get body of function as an expression.
#' 
#' This function is similar to the R \code{body} function except that function
#' returns a call object whereas this one returns an expression usable in
#' Ryacas calculations.
#' 
#' @aliases bodyAsExpression as.language
#' @param x An R function.
#' @return An expression.
#' @seealso \code{\link{body}}
#' @keywords symbolmath
#' @examples
#' 
#' 
#' # construct an R function for the Burr probability density
#' # function (PDF) given the Burr cumulative distribution function (CDF)
#' BurrCDF <- function(x, c = 1, k = 1) 1-(1+x^c)^-k
#' 
#' # transfer CDF to yacas
#' yacas(BurrCDF)
#' 
#' # create a template for the PDF from the CDF
#' BurrPDF <- BurrCDF
#' 
#' # differentiate CDF and place resulting expression in body
#' body(BurrPDF) <- yacas(expression(deriv(BurrCDF(x,c,k))))[[1]]
#' 
#' # test out PDF
#' BurrPDF(1)
#' 
bodyAsExpression <- function(x) as.expression(as.language(body(x)))

#' @export
yacas.expression <- function(x, ...) {
    x <- deparse(yparse(x), width.cutoff = 200)
    x <- gsub("\"","", x)
    .Class <- "character"
    NextMethod(x, ...)
}
   
yparse <- function(x) {
    if (!is.expression(x)) return
    options(show.error.messages = FALSE)
	# ynext does all translations, yrewrite special rewriting
    x[[1]] <- yrewrite(ynext(x[[1]]))
    options(show.error.messages = TRUE)
    x[[1]]
}

ynext <- function(x) {
    if (length(x) == 1) {
	    x <- ysub(x)
#		print(paste("1:", x))
	} else
        for (i in 1:length(x)) {
			if (length(x[[i]]) >= 1) {		
#				print(paste("x[[", i, "]]->", x[[i]]))
#				x[[i]] <- ynext(x[[i]]) 
				# Added yrewrite to make ynext really recursive
				x[[i]] <- yrewrite(ynext(x[[i]]))
#				print(paste("x[[", i, "]]->", x[[i]]))
#				print(paste(length(x), ":", x))
			}
		}
    x
}

ysub <- function(x) 
{
  if (!match(as.character(x), c("-", "+", "/", "^", "*"), nomatch = 0)) 
  {
    if (!typeof(x) == "double") 
    {
                if (match(toString(x), transtab[,"R"], nomatch = 0) >0 ) {
                        x <- trans(toString(x), from="R", to="yacas")
                        if (x == '":="') x <- ":="
                        # mode(x) <- "name"
						x <- as.name(x)
        } else if (typeof(x) == "symbol") 
            {
                try(x <- ynext(eval(x)[[1]]))
            }
    }
  }
  x
}


yrewrite <- function(x) {
    if (length(x) > 1) {
		if (x[[1]] == quote(Integrate)) {
	    	x <- yIntegrate(x)
	    }
		if (x[[1]] == quote(Deriv)) {
	    	x <- yDeriv(x)
	    }
		if (x[[1]] == quote(Limit)) {
	    	x <- yLimit(x)
	    }
		if (x[[1]] == quote(factorial)) {
	    	x <- yFactorial(x)
	    }	    
		if (x[[1]] == quote(sequence)) {
	    	x <- ySequence(x)
	    }	    
		if (x[[1]] == as.name(":=") && length(x) == 3 && 
			length(x[[3]]) > 2 &&
			x[[3]][[1]] == as.name("function")) {
		x <- yAssignFunction(x)
	    }
    }
    x
}

# Used to separatedly parse argument expressions
yUnlist <- function(x) {
	out <- c()
	if (length(x) > 1) {
		out <- paste(out, "UnList({", toString(x), "})", sep="")
	} else
		out <- paste(out, x, sep="")
}

yFactorial <- function(x) {
#	print(paste("factorial:", x))
	paste("Eval(",yUnlist(x[[2]]), ")!", sep="")
}

ySequence <- function(x) {
#	print(paste("sequence:", x))
	paste("Eval(",yUnlist(x[[2]]) ," .. ",yUnlist(x[[3]]) ,")", sep="")
}

yLimit <- function(x) {
	out <- c(); res <- ""
	res <- try(mode(eval(x[[3]])))
	if (res=="numeric") x[[3]] <- eval(x[[3]])
	out <- paste("Apply(", x[[1]], ", {", yUnlist(x[[2]]), ", Eval(",
		 yUnlist(x[[3]]), ")", sep="")
	x <- paste(out, ", ", yUnlist(x[[4]]), "})", sep="")
}

yDeriv <- function(x) {
	# tmp <- yparse(x[2][1])
	out <- c()
	# if just function name specified then add third arg
	if (length(x) == 2) x[[3]] <- "x"
	if (is.name(x[[3]])) {
		x[[3]] <- as.character(x[[3]])
	} else {
		# translate c to List
		if (identical(x[[3]][[1]], as.name("c"))) 
			x[[3]][[1]] <- as.name("List")
		# translate Deriv to D for higher order deriv
		if (identical(x[[3]][[1]], as.name("List"))) 
			x[[1]] <- "D"
		# remove quotes on variables 
		x[[3]] <- gsub('"', '', format(x[[3]]))
	}
	out <- paste("Apply(", x[[1]], ", {", format(x[[3]]), sep="")
	# if only function name specified append (x) to make F(x)
	x <- if (is.name(x[[2]])) 
		paste(out, ", ", x[[2]], "(", x[[3]], ")})", sep="")
	else
		paste(out, ", ", format(x[[2]]), "})", sep="")
}

yIntegrate <- function(x) {
	out <- c()
	if (is.name(x[[2]])) x[[2]] <- paste(x[[2]], "(x)")
	is.x.specified <- length(x) == 3 || length(x) == 5
	out <- if (is.x.specified)
		paste("Apply(", x[[1]], ", {", sep="")
	else
		paste("Apply(", x[[1]], ", {x, ", sep="")
		
	for (i in seq(3, length = length(x) - 2)) {
		if (length(x[[i]]) > 1) {
			out <- paste(out, yUnlist(x[[i]]), sep="")
		} else
			out <- paste(out, x[[i]], sep="")
		out <- paste(out, ", ", sep="")
	}
	out <- paste(out, format(x[[2]]), "})", sep="")
	out
}

	
yAssignFunction <- function(x) {
	paste(x[[2]], 
		"(", 
		paste(names(x[[3]][[2]]), collapse = ","), 
		")", 
		x[[1]], 
		format(body(eval(x[[3]]))), 
		sep = ""
	)
}

#' @export
yacas.function <- function(x, ...) {
	funname <- deparse(substitute(x))
	a <- paste( "(", paste(names(formals(x)), collapse = ","), ")" )
	b <- format(body(x))
	e <- as.expression(parse(text = b))
	s <- yparse(e)
	x <- paste(funname, a, ":=", format(s), sep = "")
	.Class <- "character"
	NextMethod(x)
}

#' @export
yacas.formula <- function(x, ...) {
	x <- as.expression(as.language(x[[length(x)]]))
	.Class <- "expression"
	NextMethod(x)
}

#' @export
yacas.yacas <- function(x, ...) {
	x <- x[[1]]
	stopifnot(is.expression(x))
	.Class <- "expression"
	NextMethod(x)
}

as.Expr.formula <- function(x) as.expression(as.language(x[[length(x)]]))



#' Evaluate a yacas expression.
#' 
#' Evaluate a yacas expression.
#' 
#' @aliases Eval Eval.default Eval.Sym Eval.Expr Eval.yacas
#' @param x Object to be evaluated.
#' @param env Environment or list in which to perform evaluation.
#' @param \dots Not currently used.
#' @keywords symbolmath
#' @examples
#' 
#' Eval(yacas(expression(x*x)), list(x=2))
#' 
#' # same
#' x <- 2
#' Eval(yacas(expression(x*x)))
#'
#' @export
Eval <- function(x, env = parent.frame(), ...) UseMethod("Eval")

#' @export
Eval.default <- function(x, env = parent.frame(), ...) {
  stop("Eval() currently only implemented for objects of type Sym, Expr and yacas")
}

#' @export
Eval.yacas <- function(x, env = parent.frame(), ...) {
  eval(x[[1]], envir = env)
}

#' @export
as.expression.yacas <- function(x, ...) x[[1]]

#' @export
as.character.yacas <- function(x, ...) as.character(x[[1]])

#' yacas interface -- silent version
#' 
#' Similar to [yacas()] but silent.
#' This can be useful when working with yacas directly.
#' 
#' @inheritParams yacas
#' @seealso yacas
#' 
#' @export
syacas <- function(x, ...) {
  invisible(yacas(x, ...))
}

#' Get width of yacas output
#' 
#' @export
get_output_width <- function() {
  res <- yacas("Print(FormulaMaxWidth())")
  res <- gsub("\n", "", res$PrettyForm, fixed = TRUE)
  res <- as.integer(res)
  return(res)
}

#' Set width of yacas output
#' 
#' @param w Width in number of characters
#' 
#' @export
set_output_width <- function(w) {
  cmd <- paste0("SetFormulaMaxWidth(", w, ")")
  return(invisible(yacas(cmd)))
}


# Matrix/vector:
# 
# Vector: List without nested lists
# Matrix: List of Lists, but no futher nesting
#
# Cannot be based on x, as this must work for e.g. Eval
# as well. So need to be based on XML.
# 
# List in yacas is "<OMS cd=\"list1\" name=\"list\"/>"
#
# x: xml string
get_xml_list_depth <- function(x) {
  
  list_depth <- Inf
  
  d <- xml2::read_xml(x)
  # Root is <OMOBJ>
  d <- xml2::xml_child(d)
  dchd <- xml2::xml_children(d)
  
  # A list?
  if (length(dchd) >= 1L && 
      isTRUE(all.equal(c(cd = "list1", name = "list"), xml2::xml_attrs(dchd[[1L]])))) {
    list_depth <- 1L
    
    # Remove list indicator
    dchd <- dchd[-1L]
    children_sublists <- rep(FALSE, length(dchd))
    num_elements <- rep(0L, length(dchd))
    
    for (j in seq_along(dchd)) {
      dchd_j <- xml2::xml_children(dchd[[j]])
      
      if (is.finite(list_depth) && 
          length(dchd_j) >= 1L && 
          isTRUE(all.equal(c(cd = "list1", name = "list"), xml2::xml_attrs(dchd_j[[1L]])))) {
        
        # Saw another list, depth 2
        list_depth <- 2L
        children_sublists[j] <- TRUE
        num_elements[j] <- length(dchd_j)
        
        # Check if there are more lists below
        dchd_j_desc <- dchd_j[-1L]
        
        if (any(grepl("<OMS cd=\"list1\" name=\"list\"/>", as.character(dchd_j_desc), fixed = TRUE))) {
          return(Inf)
        }
      }
    }
    
    # Make sure type consistent; either no sublists or all sublists
    if (length(unique(children_sublists)) > 1L) {
      return(Inf)
    }
    
    if (length(unique(num_elements)) > 1L) {
      return(Inf)
    }
  }
  
  return(list_depth)
}
