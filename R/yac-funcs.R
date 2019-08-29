#' Run yacas command returning string/character
#' 
#' @param x yacas command
#' 
#' @concept yac_communication
#' @concept yac_symbol
#' 
#' @examples 
#' yac_str("D(x) x^2 + 4*x")
#' yac_str("Limit(x, 1) (x^2 - 1)/(x - 1)")
#' yac_str("Sum(n, 1, Infinity, (1/2)^n)")
#' yac_str("Fibonacci(10)")
#' yac_str("Sum(n, 1, 10, Fibonacci(n))")
#' yac_str("TeXForm(x^2 - 1)")
#' 
#' @export
yac_str <- function(x) {
  UseMethod("yac_str")
}

#' @export
yac_str.default <- function(x) {
  z <- yac_core(x)

  # Side effects, mainly by e.g. PrettyForm():
  if (nchar(z[1L]) > 0) {
    return(z[1L])
    #cat(z[1L])
    #return(invisible(z[1L]))
  }
  
  z <- z[2L]
  z <- strip_start_end_quotes(z)
  
  # TeXForm:
  z <- gsub("^[$ ]+(.*)[$ ]+$", "\\1", z)
  
  return(z)
}


#' Run yacas command returning R expression
#' 
#' @param x yacas command
#' 
#' @concept yac_communication
#' @concept yac_symbol
#' 
#' @examples 
#' yac_expr("D(x) x^2 + 4*x")
#' yac_expr("Limit(x, 1) (x^2 - 1)/(x - 1)")
#' yac_expr("Sum(n, 1, Infinity, (1/2)^n)")
#' yac_expr("Fibonacci(10)")
#' yac_expr("Sum(n, 1, 10, Fibonacci(n))")
#' 
#' @export
yac_expr <- function(x) {
  UseMethod("yac_expr")
}

#' @export
yac_expr.default <- function(x) {
  z <- yac_core(paste0("RForm(", x, ")"))
  z <- z[2L]
  z <- strip_start_end_quotes(z)
  
  z <- parse(text = z, keep.source = FALSE)
  
  return(z)
}


#' Run yacas command silently
#' 
#' @param x yacas command
#' 
#' @concept yac_communication
#' @concept yac_symbol
#' 
#' @export
yac_silent <- function(x) {
  UseMethod("yac_silent")
}

#' @export
yac_silent.default <- function(x) {
  z <- yac_str(x)
  return(invisible(z))
}


#' Run yacas command
#' 
#' @param x yacas command
#' @param rettype `str` for string/character, `expr` for expression, `silent` for silent
#' 
#' @concept yac_communication
#' @concept yac_symbol
#' 
#' @examples
#' yac("D(x) x^2 + 4*x")
#' yac("D(x) x^2 + 4*x", rettype = "str")
#' yac("D(x) x^2 + 4*x", rettype = "expr")
#' yac("D(x) x^2 + 4*x", rettype = "silent")
#' 
#' @export
yac <- function(x, rettype = c("str", "expr", "silent")) {
  UseMethod("yac")
}

#' @export
yac.default <- function(x, rettype = c("str", "expr", "silent")) {
  rettype <- match.arg(rettype)
  
  if (rettype == "str") {
    return(yac_str(x))
  } else if (rettype == "expr") {
    return(yac_expr(x))
  } else if (rettype == "silent") {
    return(yac_silent(x))
  }
  
  stop("Unknown rettype")
}





#' Assign yacas variable
#' 
#' @param value Expression
#' @param x Variable name
#' 
#' @concept yac_communication
#' @concept yac_symbol
#' 
#' @export
yac_assign <- function(value, x) {
  UseMethod("yac_assign")
}

#' @export
yac_assign.default <- function(value, x) {
  return(yac_silent(paste0(x, " := ", value)))
}


