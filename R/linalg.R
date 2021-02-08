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

#' Matrix Power
#'
#' @param x If `yac_symbol` treat as such, else call [pow.default()].
#' @param n `n`th power of the square matrix.
#' @param \dots further arguments passed to [pow.default()]
#'
#' @concept yac_symbol
#'
#' @examples
#' (x <- matrix(1:9, ncol = 3))
#' pow(x, 2)
#' pow(ysym(x), 2)
#' @export
pow <- function(x, n, ...) {
  UseMethod("pow")
}

#' @export
pow.default <- function(x, n, ...) {
  stopifnot(is.numeric(x)) # numeric
  stopifnot(is.matrix(x)) # matrix
  stopifnot(dim(x)[1] == dim(x)[2]) # square
  n <- as.integer(n) # coerce n to integer
  if (n == 0) {
    return(diag(dim(x)[1])) # identity matrix
  }
  if (n < 0) {
    # for negative power
    # get the inverse first before matrix multiplication
    x <- solve(x)
    n <- abs(n)
  }
  if (n == 1) {
    return(x)
  }
  # out is a placeholder
  out <- x
  iter <- n - 1
  for (i in 1:iter) {
    out <- out %*% x
  }
  return(out)
}

#' @export
pow.yac_symbol <- function(x, n, ...) {
  stopifnot(methods::is(x, "yac_symbol"))

  y_res <- yac_str(x$yacas_cmd)
  y <- ysym(y_res)

  stopifnot(y$is_mat)

  return(y_fn(x, "MatrixPower", n))
}
