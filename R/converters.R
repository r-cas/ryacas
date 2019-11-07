#' Convert R vector/matrix to yacas vector (list) or matrix (list of lists)
#' 
#' @param x R vector to convert
#' 
#' @concept helper
#' 
#' @export
as_y <- function(x) {
  UseMethod("as_y")
}

#' @export
as_y.default <- function(x) {
  if (is.matrix(x)) {
    return(y_hlp_to_yacmat(x))
  }
  
  if (is.vector(x) && length(x) == 1L) {
    return(x)
  }
  
  return(y_hlp_to_yacvec(x))
}

#' Convert yacas object to R
#' 
#' If `x` is a yacas command as string, convert to a character vector/matrix in R.
#' If `x` is a `yac_symbol` (e.g. from [ysym()]), then convert it to a numeric object 
#' if there are no variables or a character type if there are variables.
#' 
#' In yacas a vector is a list, and a matrix is a list of lists.
#' 
#' @param x yacas list or list of lists to convert
#' 
#' @concept helper
#' 
#' @export
as_r <- function(x) {
  UseMethod("as_r")
}


expr_has_vars <- function(x) {
  y_vars <- all.vars(x)
  
  # Known "vars"
  y_vars <- setdiff(y_vars, 
                    c("pi"))
  
  if (length(y_vars) > 0L) {
    return(TRUE)
  }
  
  return(FALSE)
}

#' @export
as_r.default <- function(x) {
  # Two {'s in line, potentially with space in between
  if (grepl("\\{[ ]*\\{", x)) {
    # Matrix
    return(y_hlp_from_yacmat(x))
  }
  
  # One {
  if (grepl("\\{", x)) {
    # Vector
    return(y_hlp_from_yacvec(x))
  }
  
  y <- yac_expr(x)
  
  if (expr_has_vars(y)) {
    return(y)
  }
  
  # No variables, just eval
  return(eval(y))
}

