
#
# yacas is in YACAS_HOME. If unset then on Windows the yacas
# distributed with R is used and otherwise the yacas on the
# path is used.  If YACAS_HOME is a directory, filename
# is assumed to be yacas.exe on Windows and yacas otherwise.
#
# scripts are in YACAS_SCRIPTS or in yacas home directory
# if not specified.  If YACAS_SCRIPTS is a directory, filename 
# is assumed to be scripts.dat .
#
# init file is in YACAS_INIT or in yacdir within the Ryacas
# package if not specified.  If YACAS_INIT is a directory, filename 
# is assumed to be R.ys.

yacasInvokeString <- function(method = c("socket", "system")) {
   method <- match.arg(method)
   whole.path <- function(path, defpath, deffile) {
      if (path == "") path <- defpath
      if (file.info(path)$isdir) file.path(path, deffile) else path
   }

   if (.Platform$OS.type == "windows") {
      yacas.args <- "-pc"
      yacas <- whole.path(Sys.getenv("YACAS_HOME"),
      	 system.file(package = "Ryacas"), "yacdir/yacas.exe")
      yacas.init <- whole.path(Sys.getenv("YACAS_INIT"),
        system.file(package = "Ryacas"), "yacdir/R.ys")
      yacas.post <- ""
      yacas.scripts <- whole.path(Sys.getenv("YACAS_SCRIPTS"), 
        dirname(yacas), "scripts.dat")
      yacas.scripts <- paste("--archive", shQuote(yacas.scripts))
   } else {
      yacas.args <- "-pc"
      yacas <- "yacas"
      yacas.init <- file.path(system.file(package = "Ryacas"), "yacdir/R.ys")
      yacas.post <- "  "
      yacas.scripts <- ""
   }

   server.string <- if(method == "socket") "--server 9734" else ""
   paste(yacas, "--init", shQuote(yacas.init), 
                            yacas.scripts, yacas.args,
                            server.string, yacas.post)
}

haveYacas <- function () 
  !suppressWarnings(yacas("quit", method = "system", 
    retclass = "character"))

yacasStart <- function(verbose = FALSE)
{
  if (!capabilities("sockets")) stop("no socket capabilties")
  yacasStop(verbose = FALSE)
  cmd.str <- yacasInvokeString()
  print("Starting Yacas!")
   # return path using defpath and deffile as fill-in defaults

  if (verbose)
     cat("Invoking Yacas with command line:\n   ", cmd.str, "\n")
  if (.Platform$OS.type == "windows") {
    system(cmd.str, wait = FALSE)
  } else {
    system(paste(cmd.str, "&"))
  }

  .yacCon<<-socketConnection(host = "127.0.0.1", port=9734, server = FALSE,
                      blocking = FALSE, open = "a+",
                      encoding = getOption("encoding"))
  Sys.sleep(1)
  invisible(0)
}

isConnection <- function(x) {
	opened <- summary(x)$opened
	identical(opened, "opened") || identical(opened, "closed")
}

yacasStop <- function(verbose = TRUE) 
{
  if (exists(".yacCon", .GlobalEnv)) {
      if (isConnection(get(".yacCon", .GlobalEnv))) try(close(.yacCon))
      rm(.yacCon, envir = .GlobalEnv)
  }
  if (.Platform$OS.type == "windows") system("taskkill /im yacas.exe")
  if (verbose) cat("Thank you for using yacas\n")
  return(invisible(0))
}
.Last.lib <- function(lib) 
{
  if (exists(".yacCon", .GlobalEnv)) yacasStop()
  # next statement has no effect except on Windows XP Pro
}

# proper counting of lines read in, and proper handling of them.

yacas <- function(x, verbose = FALSE, method = c("socket", "system"), ...)
  UseMethod("yacas")


yacas.character <- function(x, verbose = FALSE, method = c("socket", "system"), retclass = c("expression", "character"), addSemi = TRUE, ...) {

    addSemiFn <- function(x) {
      x <- sub(";[[:blank:]]*$", "", x)
      x <- paste(x, ";", collapse='', sep='')
      return(x)
    }

    retclass = match.arg(retclass)
    method <- match.arg(method)

    if (retclass == "expression") {
    	x <- paste("Eval(",x,");")
    } else {
		if (addSemi) x <- addSemiFn(x)
	}
	
    if (method == "system")
        if (.Platform$OS.type == "windows")
            return(system(yacasInvokeString(method = "system"), 
		          input = x, invisible = TRUE))
		else {
            f.tmp = file.path(tempdir(), ".R/yacas.tmp")
            if (!file.create(f.tmp)) {
                warning("cannot create tmp yacas input file")
                return(FALSE)
            }
            out <- file(f.tmp, open = "a")
#            cat(paste("Echo('Executing :'", x, ");"))
            cat(x, file=out)
            close(out)
            return(system(paste(yacasInvokeString(method = "system"), f.tmp))) 
        }

    # if connection does not exist or its not a connection
    # or its closed, startup Yacas.
    if (!exists(".yacCon", .GlobalEnv) ||
	!isConnection(get(".yacCon", .GlobalEnv)) ||
	summary(get(".yacCon", .GlobalEnv))$opened == "closed")
	    yacasStart(verbose = verbose)

    yac.res <- c()
#	print(x)
    writeLines(x, .yacCon)

    delim <- "]"
    yac.res <- c()
    while (sum(delim == yac.res) < 2)
    {
      yac.out <- readLines(.yacCon)
      yac.res <- c(yac.res, yac.out)
    }
    yac.res <- yac.res[yac.res != ""]
    flush(.yacCon)

    # print all non-delims in verbose mode
    is.delim <- yac.res == delim
    # print non-delims
    if (any(!is.delim) && verbose) print(yac.res[!is.delim]) 

    w <- which(is.delim)[1:2]
    chunk1 <- yac.res[seq(1, length = w[1]-1)]
    chunk2 <- yac.res[seq(w[1]+1, length = diff(w)-1)]
    if (yac.res[1] == "<OMOBJ>") {
	result <- list(parse(text = OpenMath2R(chunk1)), OMForm = chunk1)
    } else if (length(chunk1) > 0) result <- list(NULL, PrettyForm = chunk1)
    else result <- list(NULL, YacasForm = chunk2)
    class(result) <- "yacas"
    result
}

# test
# yacas("Pi+Sin(x)", retclass = "character", OM=TRUE)



as.language <- function(x) parse(text=paste(deparse(x)))[[1]]
bodyAsExpression <- function(x) as.expression(as.language(body(x)))
	
yacas.expression <- function(x, verbose = FALSE, method = c("socket", "system"), retclass = c("expression", "character"), ...) {
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
    if (length(x) == 1)
            x <- ysub(x)
    else
        for (i in 1:length(x)) 
            if (length(x[[i]]) >= 1) 
                x[[i]] <- ynext(x[[i]])
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
                        mode(x) <- "name"
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
		if (x[[1]] == quote(Factorial)) {
	    	x <- yFactorial(x)
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
	paste("Eval(",yUnlist(x[[2]]), ")!", sep="")
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

yacas.function <- function(x, verbose = FALSE, method = c("socket", "system"), ...) {
	funname <- deparse(substitute(x))
	a <- paste( "(", paste(names(formals(x)), collapse = ","), ")" )
	b <- format(body(x))
	s <- paste(funname, a, ":=", b, sep = "")
	x <- as.expression(parse(text = s))
	.Class <- "expression"
	NextMethod(x)
}

