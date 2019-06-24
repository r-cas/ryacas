#' Prepare simple `yacas` call
#' 
#' @param x parameter to function `fn`
#' @param fn function with parameter `x`
#' @param \dots additional arguments to `fn`
#' 
#' @examples
#' y_fn("x^2 - 1", "Factor")
#' 
#' cmd <- "x^2 - 1 == 0" %>% y_fn("Solve", "x")
#' cmd
#' yac_str(cmd)
#' yac_solve_str(cmd)
#' 
#' @concept helper
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

