#' Make a yacas symbol
#' 
#' Note that this results in multiple calls to `yacas` and 
#' the performance may be slower than manually using e.g. [yac_str()].
#' 
#' @param x A vector or a matrix
#' 
#' @return A `yac_symbol`
#' 
#' @concept yac_symbol
#' @export
yac_symbol <- function(x) {
  stopifnot(is.vector(x) | is.matrix(x))
  
  cmd <- if (is.matrix(x) | length(x) > 1L) {
    as_y(x)
  } else {
    x
  }
  
  cmd <- as.character(cmd)
  # NOTE: Do this here or later?
  cmd <- yac_str(cmd)
  
  is_mat <- grepl("^[ ]*\\{[ ]*\\{.*\\}[ ]*\\}[ ]*$", cmd)
  is_vec <- !is_mat && grepl("^[ ]*\\{.*\\}[ ]*$", cmd)
  
  y <- list(yacas_cmd = cmd,
            is_mat = is_mat,
            is_vec = is_vec)
  
  class(y) <- c("yac_symbol", "list")
  return(y)
}

# S3 exports

#' @export
yac_str.yac_symbol <- function(x) {
  return(yac_str(x$yacas_cmd))
}

#' @export
yac_expr.yac_symbol <- function(x) {
  return(yac_expr(x$yacas_cmd))
}

#' @export
yac_silent.yac_symbol <- function(x) {
  return(yac_silent(x$yacas_cmd))
}

#' @export
yac.yac_symbol <- function(x, rettype = c("str", "expr", "silent")) {
  return(yac(x$yacas_cmd, rettype))
}

#' @export
yac_assign.yac_symbol <- function(value, x) {
  return(yac_silent(paste0(x, " := ", value$yacas_cmd)))
}

#' @export
y_fn.yac_symbol <- function(x, fn, ...) {
  x <- y_fn(x$yacas_cmd, fn, ...)
  y <- yac_str(x)
  z <- yac_symbol(y)
  return(z)
}



#' @export
as_r.yac_symbol <- function(x) {
  y <- yac_expr(x$yacas_cmd)

  if (expr_has_vars(y)) {
    return(y)
  }

  return(eval(y))
}

#' @export
dim.yac_symbol <- function(x) {
  if (!x$is_mat) {
    return(NULL)
  }
  
  #rows:=Length(matrix);
  rows <- as.integer(yac_str(y_fn(x$yacas_cmd, "Length")))
  
  # cols:=Length(matrix[1]);
  cols <- as.integer(yac_str(y_fn(paste0(x$yacas_cmd, "[1]"),"Length")))
  
  return(c(rows, cols))
}

#' @export
length.yac_symbol <- function(x) {
  if (!(x$is_vec || x$is_mat)) {
    return(NULL)
  }
  
  if (x$is_vec) {
    return(as.integer(yac_str(y_fn(x$yacas_cmd, "Length"))))
  }
  
  return(prod(dim(x)))
}

#' Simplify expression
#' 
#' @param x A `yac_symbol`
#' @param timeout timeout in seconds before simplification is aborted; 
#' only works when package unix is available
#' 
#' @concept yac_symbol
#' 
#' @export
simplify <- function(x, timeout = 2) {
  UseMethod("simplify")
}

#' @export
simplify.yac_symbol <- function(x, timeout = 2) {
  stopifnot(is.null(timeout) || (!is.null(timeout) && timeout > 0))
  
  x_res <- yac_symbol(yac_str(x$yacas_cmd))
  z <- y_fn(x_res$yacas_cmd, "Simplify")
  
  z_res <- NULL
  
  # unix available
  if (!is.null(timeout) && requireNamespace("unix", quietly = TRUE)) {
    z_res <- unix::eval_safe(yac_str(z), timeout = timeout)
  } else {
    z_res <- yac_str(z)
  }
  
  stopifnot(!is.null(z_res))
  
  v <- yac_symbol(z_res)
  
  return(v)
}

#' Export object to TeX
#' 
#' @param x A `yac_symbol`
#' 
#' @concept yac_symbol
#' 
#' @export
tex <- function(x) {
  UseMethod("tex")
}

#' @export
tex.yac_symbol <- function(x) {
  x_res <- yac_symbol(yac_str(x$yacas_cmd))
  
  z <- y_fn(x_res$yacas_cmd, "TeXForm")
  z_res <- yac_str(z)
  
  return(z_res)
}

#' Matrix multiplication
#' 
#' @param x A `yac_symbol`
#' @param y A `yac_symbol`
#' 
#' @concept yac_symbol
#' 
#' @export
`%*%` <- function(x, y) {
  UseMethod("%*%")
}

#' @export
`%*%.default` <- function(x, y) {
  return(base::`%*%`(x, y))
}

#' @export
`%*%.yac_symbol` <- function(x, y) {
  stopifnot(methods::is(x, "yac_symbol"))
  stopifnot(methods::is(y, "yac_symbol"))
  
  x_res <- yac_symbol(yac_str(x$yacas_cmd))
  y_res <- yac_symbol(yac_str(y$yacas_cmd))
  
  z <- paste0(x_res$yacas_cmd, " * ", y_res$yacas_cmd)
  z_res <- yac_str(z)
  
  v <- yac_symbol(z_res)
  
  return(v)
}

#' Matrix diagonals
#' 
#' From [base::diag()].
#' 
#' @param x If `yac_symbol` treat as such, else call [base::diag()].
#' @param \dots further arguments passed to [base::diag()]
#' 
#' @concept yac_symbol
#' 
#' @export
diag <- function(x, ...) {
  UseMethod("diag")
}

#' @export
diag.default <- function(x, ...) {
  return(base::diag(x = x, ...))
}

#' @export
diag.yac_symbol <- function(x, ...) {
  stopifnot(methods::is(x, "yac_symbol"))
  
  y_res <- yac_str(x$yacas_cmd)
  y <- yac_symbol(y_res)
  
  stopifnot(y$is_mat)
  
  w <- as_r(y$yacas_cmd)
  z <- base::diag(w, ...)
  v <- yac_symbol(as_y(z))
  
  return(v)
}




#' Matrix diagonals
#' 
#' From [base::diag()].
#' 
#' @param x If `yac_symbol` treat as such, else call `base::diag<-()`.
#' @param value New value for `diag(x)`
#' 
#' @concept yac_symbol
#' 
#' @export
`diag<-` <- function(x, value) {
  UseMethod("diag<-")
}

#' @export
`diag<-.default` <- function(x, value) {
  return(base::`diag<-`(x = x, value = value))
}

#' @export
`diag<-.yac_symbol` <- function(x, value) {
  stopifnot(methods::is(x, "yac_symbol"))

  y_res <- yac_str(x$yacas_cmd)
  y <- yac_symbol(y_res)
  stopifnot(y$is_mat)

  w <- as_r(y$yacas_cmd)
  
  z <- base::`diag<-`(w, value)
  
  v <- yac_symbol(as_y(z))
  
  return(v)
}


#' Lower and upper triangular part of a matrix
#' 
#' @param x If `yac_symbol` treat as such, else 
#' call [base::lower.tri()]/[base::upper.tri()].
#' @param diag Whether diagonal is included.
#' 
#' @concept yac_symbol
#' 
#' @export
upper.tri <- function(x, diag = FALSE) {
  UseMethod("upper.tri")
}

#' @export
upper.tri.default <- function(x, diag = FALSE) {
  return(base::upper.tri(x = x, diag = diag))
}

#' @export
upper.tri.yac_symbol <- function(x, diag = FALSE) {
  stopifnot(methods::is(x, "yac_symbol"))
  
  y_res <- yac_str(x$yacas_cmd)
  y <- yac_symbol(y_res)
  
  stopifnot(y$is_mat)
  
  w <- as_r(y$yacas_cmd)
  return(base::upper.tri(x = w, diag = diag))
}

#' Lower and upper triangular part of a matrix
#' 
#' @param x If `yac_symbol` treat as such, else 
#' call [base::lower.tri()]/[base::upper.tri()].
#' @param diag Whether diagonal is included.
#' 
#' @concept yac_symbol
#' 
#' @export
lower.tri <- function(x, diag = FALSE) {
  UseMethod("lower.tri")
}

#' @export
lower.tri.default <- function(x, diag = FALSE) {
  return(base::lower.tri(x = x, diag = diag))
}

#' @export
lower.tri.yac_symbol <- function(x, diag = FALSE) {
  stopifnot(methods::is(x, "yac_symbol"))
  
  y_res <- yac_str(x$yacas_cmd)
  y <- yac_symbol(y_res)
  
  stopifnot(y$is_mat)
  
  w <- as_r(y$yacas_cmd)
  return(base::lower.tri(x = w, diag = diag))
}


#' @title t
#' @name t
#' 
#' @param x If `yac_symbol` treat as such, else 
#' call [base::t()].
#' 
#' @concept yac_symbol
#' @export
t.yac_symbol <- function(x) {
  # To see if x is indeed a matrix
  y_res <- yac_str(x$yacas_cmd)
  y <- yac_symbol(y_res)
  
  stopifnot(y$is_mat)
  
  z <- y_fn(x = y_res, fn = "Transpose")
  v <- yac_symbol(yac_str(z))
  
  return(v)
}

#' Solve a system of equations
#' 
#' This generic function solves the 
#' equation $a %*% x = b$ for $x$.
#' 
#' @param a If `yac_symbol` treat as such, else 
#' call [base::solve()].
#' @param b If `yac_symbol` treat as such, else 
#' call [base::solve()].
#' @param \dots Not used
#' 
#' @export
solve.yac_symbol <- function(a, b, ...) {
  # FIXME: Currently Inverse, but implement general case
  if (!missing(b)) {
    stop("solve() currently only supports finding inverse")
  }
  
  # To see if x is indeed a matrix
  y_res <- yac_str(a$yacas_cmd)
  y <- yac_symbol(y_res)

  stopifnot(y$is_mat)
  
  z <- y_fn(x = y, fn = "Inverse")
  z_res <- yac_str(z)
  v <- yac_symbol(z_res)
  
  return(v)
}


#' Extract or replace parts of an object
#' 
#' @param x A `yac_symbol`.
#' @param i row indices specifying elements to extract or replace
#' @param j column indices specifying elements to extract or replace
#' 
#' @export
`[.yac_symbol` <- function(x, i, j) {
  stopifnot(methods::is(x, "yac_symbol"))
  
  y_res <- yac_str(x$yacas_cmd)
  y <- yac_symbol(y_res)
  
  stopifnot(y$is_mat | y$is_vec)
  
  if (y$is_vec && !missing(j)) {
    stop("Cannot specify second dimension for a vector")
  }
  
  w <- as_r(y$yacas_cmd)
  z <- NULL
  
  if (y$is_vec) {
    z <- base::`[`(x = w, i = i)
  } else if (y$is_mat) {
    if (missing(j)) {
      n_args <- nargs()
      
      if (n_args == 2L) {
        # x[1:2]
        z <- base::`[`(x = w, i = i)
      } else if (n_args == 3L) {
        # x[1:2,]
        z <- base::`[`(x = w, i = i, )
      }
    } else {
      z <- base::`[`(x = w, i = i, j = j)
    }
  }
  
  stopifnot(!is.null(z))
  v <- yac_symbol(as_y(z))
  
  return(v)
}

#' Extract or replace parts of an object
#' 
#' @param x A `yac_symbol`.
#' @param i row indices specifying elements to extract or replace
#' @param j column indices specifying elements to extract or replace
#' @param value the value to replace `x[i, j]` by
#' 
#' @export
`[<-.yac_symbol` <- function(x, i, j, value) {
  # NOTE: 
  # quote? To enable e.g.
  # x[i, j] <- a
  # for some symbol a

  stopifnot(methods::is(x, "yac_symbol"))
  
  y_res <- yac_str(x$yacas_cmd)
  y <- yac_symbol(y_res)
  
  stopifnot(y$is_mat | y$is_vec)
  
  if (y$is_vec && !missing(j)) {
    stop("Cannot specify second dimension for a vector")
  }
  
  w <- as_r(y$yacas_cmd)
  z <- NULL

  if (y$is_vec) {
    z <- `[<-`(x = w, i = i, value = value)
  } else if (y$is_mat) {
    if (missing(j)) {
      n_args <- nargs()

      if (n_args == 3L) {
        # x[1:2] <- ...
        z <- base::`[<-`(x = w, i = i, value = value)
      } else if (n_args == 4L) {
        # x[1:2,] <- ...
        allj <- seq_len(ncol(w))
        z <- base::`[<-`(x = w, i = i, j = allj, value = value)
      }
      
    } else {
      z <- `[<-`(x = w, i = i, j = j, value = value)
    }
  }
  
  stopifnot(!is.null(z))
  v <- yac_symbol(as_y(z))
  
  return(v)
}

#' @export
print.yac_symbol <- function(x, ...) {
  y_res <- yac_str(x$yacas_cmd)
  #cat("Yacas symbol: ", yac_str(x$yacas_cmd), "\n", sep = "")
  Ryacas::y_print(y_res)
  return(invisible(x))
}

#' @importFrom utils str
#' @export
str.yac_symbol <- function(object, ...) {
  x <- object
  class(x) <- "list"
  return(str(x))
}

#' Math operators
#' 
#' @param e1 A `yac_symbol`.
#' @param e2 A `yac_symbol`.
#' 
#' @export
Ops.yac_symbol = function(e1, e2) {
  if (!(.Generic %in% c("+", "-", "*", "/", "^"))) {
    stop("Function '", .Generic, "' not yet implemented for yac_symbol")
  } 
  
  # LHS constant, e.g. 2*x: e1 is a number 2
  if (.Method[1] == "") {
    e1 <- yac_symbol(e1)
  }
  
  # RHS constant, e.g. x*2: e2 is a number 2
  if (.Method[2] == "") {
    e2 <- yac_symbol(e2)
  }
  
  txt <- paste0("(", e1$yacas_cmd, ")", .Generic, "(", e2$yacas_cmd, ")")
  txt_res <- yac_str(txt)
  x <- yac_symbol(txt_res)
  
  return(x)
}

Math_transtab <- matrix( c(
  #R					yacas
  "sin",			"Sin",
  "cos",			"Cos",
  "tan",			"Tan",
  
  "asin",	  	"ArcSin",
  "acos",	  	"ArcCos",
  "atan",    	"ArcTan",
  "asinh", 	  "ArcSinh", 
  "acosh", 	  "ArcCosh", 
  "atanh",   	"ArcTanh",
  
  "exp", 	  	"Exp",
  "log", 	  	"Ln",
  "sqrt", 	  "Sqrt"
), byrow = TRUE, ncol = 2)
colnames(Math_transtab) <- c("R", "yacas")
#paste0(Math_transtab[, 1], "()", collapse = ", ")

#' Math functions
#' 
#' @param x `yac_symbol`.
#' @param \dots further arguments passed to methods
#' 
#' @export
Math.yac_symbol = function(x, ...) {
  i <- match(.Generic, Math_transtab[, 1L])
  
  if (is.na(i)) {
    stop("Function '", .Generic, "' not yet implemented for yac_symbol")
  } 
  
  fn <- Math_transtab[i, 2L]
  
  txt <- paste0(fn, "(", x$yacas_cmd, ")")
  x <- yac_symbol(txt)
  return(x)
}
