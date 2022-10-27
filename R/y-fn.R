#' Prepare simple `yacas` call
#' 
#' @param x parameter to function `fn`
#' @param fn function with parameter `x`
#' @param \dots additional arguments to `fn`
#' 
#' @examples
#' y_fn("x^2 - 1", "TeXForm")
#' yac_str(y_fn("x^2 - 1", "TeXForm"))
#' 
#' y_fn("x^2 - 1", "Factor")
#' yac_str(y_fn("x^2 - 1", "Factor"))
#' 
#' cmd <- "x^2 - 1 == 0" %>% y_fn("Solve", "x")
#' cmd
#' sol <- yac_str(cmd)
#' sol
#' yac_str(y_rmvars(sol))
#' 
#' @concept helper
#' @concept yac_symbol
#' 
#' @export
y_fn <- function(x, fn, ...) {
  UseMethod("y_fn")
}

#' @export
y_fn.default <- function(x, fn, ...) {
  args <- list(...)

  if (length(args) == 0) {
    return(paste0(fn, "(", x, ")"))
  }
  
  extra_args <- paste0(unlist(args), collapse = ", ")

  return(paste0(fn, "(", x, ", ", extra_args, ")"))
}

#' Remove/strip variable names
#' 
#' This only builds a yacas command. 
#' You need to also call [yac_str()], [yac_expr()] or similar. 
#' This is the reason that it does not call yacas: it depends on how you 
#' want it returned (string, expression).
#' 
#' @param x yacas command
#' 
#' @concept helper
#' 
#' @examples
#' cmd <- "{x == 2, y == 4}"
#' yac_str(cmd)
#' yac_str(y_rmvars(cmd))
#' 
#' @export
y_rmvars <- function(x) {
  UseMethod("y_rmvars")
}

#' @export
y_rmvars.default <- function(x) {
  # FIXME: Best API?
  paste0("((", x, ") /:: { _lhs == _rhs <- rhs })")
}


#' Evaluate a yacas expression
#' 
#' Evaluate a yacas expression by replacing variables with values as for the
#' given list.
#'
#' @param expr a valid yacas expression
#' 
#' @param ... a list of assignements (see example)
#'
#' @concept helper
#' 
#' @examples
#' eq <- ysym("2*y+x^2+2*x-3")
#' y_eval(eq, x=3, y=2)
#'
#' @export
y_eval <- function(expr, ...) {
  UseMethod("y_eval")
}

#' @export
y_eval.default <- function(expr, ...) {
  args <-  list(...)
  assign <- c()
  for (v in names(args)) {
    assign <- c(assign, paste0(v, "==", args[[v]]))
  }
  y_fn(expr, "Where", paste(assign, collapse=" And "))
}