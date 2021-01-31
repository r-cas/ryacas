det.yac_symbol <- function(expr, ...) {
  vars <- unlist(list(...))
  
  res <- paste0("Determinant(", expr$yacas_cmd, ")")
  
  res_sym <- ysym(res)
  return(res_sym)
}

tr.yac_symbol <- function(expr, ...) {
  vars <- unlist(list(...))
  
  res <- paste0("Trace(", expr$yacas_cmd, ")")
  
  res_sym <- ysym(res)
  return(res_sym)
}
