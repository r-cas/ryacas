#' Matrix Determinant
#'
#' From [base::det()].
#'
#' @param x If `yac_symbol` treat as such, else call [base::det()].
#' @param \dots further arguments passed to [base::det()]
#'
#' @concept yac_symbol
#'
#' @examples
#' (x <- matrix(1:4, ncol = 2))
#' det(x)
#' det(ysym(x))
#' @export
det <- function(x, ...) {
  UseMethod("det")
}

#' @export
det.default <- function(x, ...) {
  return(base::det(x = x, ...))
}

#' @export
det.yac_symbol <- function(x, ...) {
  stopifnot(methods::is(x, "yac_symbol"))

  y_res <- yac_str(x$yacas_cmd)
  y <- ysym(y_res)

  stopifnot(y$is_mat)

  return(y_fn(x, "Determinant"))
}


#' Matrix Trace
#'
#' The trace of a square matrix is the sum of the diagonal elements.
#' 
#' @param x If `yac_symbol` treat as such, else call [tr.default()].
#' @param \dots further arguments passed to [tr.default()]
#'
#' @concept yac_symbol
#'
#' @examples
#' (x <- matrix(1:4, ncol = 2))
#' tr(x)
#' tr(ysym(x))
#' @export
tr <- function(x, ...) {
  UseMethod("tr")
}

#' @rdname tr
#' @export
tr.default <- function(x, ...) {
  stopifnot(is.numeric(x)) # numeric
  stopifnot(is.matrix(x)) # matrix
  stopifnot(dim(x)[1] == dim(x)[2]) # square
  return(sum(diag(x)))
}

#' @export
tr.yac_symbol <- function(x, ...) {
  stopifnot(methods::is(x, "yac_symbol"))

  y_res <- yac_str(x$yacas_cmd)
  y <- ysym(y_res)

  stopifnot(y$is_mat)

  return(y_fn(x, "Trace"))
}
