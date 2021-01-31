det.default <- function(x, ...) {
  return(base::det(x))
}

det.yac_symbol <- function(expr, ...) {
  vars <- unlist(list(...))
  
  res <- paste0("Determinant(", expr$yacas_cmd, ")")
  
  res_sym <- ysym(res)
  return(res_sym)
}

tr.default <- function(x, ...) {
  return(
    sum(diag(x))
  )
}

tr.yac_symbol <- function(expr, ...) {
  vars <- unlist(list(...))
  
  res <- paste0("Trace(", expr$yacas_cmd, ")")
  
  res_sym <- ysym(res)
  return(res_sym)
}
