
yacas <- function(x, ...)
  UseMethod("yacas")

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

    if (grepl('^<OMOBJ>', yacas.res[1])) {
        text <- OpenMath2R(yacas.res[1])

        if (retclass == "expression")
            text <- parse(text = text, srcfile = NULL)
        else if (retclass == "unquote")
            text <- sub("^['\"](.*)['\"]", "\\1", text)

        result <- list(text = text, OMForm = yacas.res[1])
    } else if (nchar(yacas.res[1]) > 0) {
        result <- list(NULL, PrettyForm = sub('<OMOBJ>.*</OMOBJ>(\r?\n)', '', yacas.res[1]))
    } else {
        result <- list(NULL, YacasForm = yacas.res[2])
    }

    class(result) <- "yacas"
    result
}

# test
# yacas("Pi+Sin(x)", retclass = "character", OM=TRUE)



as.language <- function(x) parse(text=paste(deparse(x)))[[1]]
bodyAsExpression <- function(x) as.expression(as.language(body(x)))
	
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

yacas.formula <- function(x, ...) {
	x <- as.expression(as.language(x[[length(x)]]))
	.Class <- "expression"
	NextMethod(x)
}

yacas.yacas <- function(x, ...) {
	x <- x[[1]]
	stopifnot(is.expression(x))
	.Class <- "expression"
	NextMethod(x)
}

as.Expr.formula <- function(x) as.expression(as.language(x[[length(x)]]))

Eval <- function(x, env = parent.frame(), ...) UseMethod("Eval")

Eval.yacas <- function(x, env = parent.frame(), ...) 
	eval(x[[1]], envir = env)

as.expression.yacas <- function(x, ...) x[[1]]
as.character.yacas <- function(x, ...) as.character(x[[1]])

