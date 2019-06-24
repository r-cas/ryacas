solve_build_cmd <- function(x, reduce_var, rform = FALSE) {
  cmd_f <- paste0("func(zzzzz) := Eval(", reduce_var, " Where zzzzz)")
  
  if (rform) {
    cmd <- paste0("[ Local(func); Local(funcres); ", cmd_f, "; funcres := MapSingle(\"func\", ", x, "); RForm(funcres); ];")
    return(cmd)
  }
  
  cmd <- paste0("[ Local(func); ", cmd_f, "; MapSingle(\"func\", ", x, "); ];")
  return(cmd)
}

check_solve <- function(cmd) {
  if (!grepl("^[ ]*Solve", cmd)) {
    stop("The command does not start with 'Solve'")
  }
}

extract_reduce_var <- function(cmd) {
  var <- gsub("^[ ]*Solve[ ]*\\(.*,[ ]*(.+)[ ]*\\)[ ]*", "\\1", cmd)
  return(var)
}

#' Run yacas Solve() command returning string/character
#' 
#' @param x yacas command starting with a Solve()
#' 
#' @concept solve
#' 
#' @export
yac_solve_str <- function(x) {
  check_solve(x)
  reduce_var <- extract_reduce_var(x)
  
  cmd <- solve_build_cmd(x = x, reduce_var = reduce_var, rform = FALSE)
  return(yac_str(cmd))
}

#' Run yacas Solve() command returning R expression
#' 
#' @param x yacas command starting with a Solve()
#' 
#' @concept solve
#' 
#' @export
yac_solve_expr <- function(x) {
  check_solve(x)
  reduce_var <- extract_reduce_var(x)
  
  cmd <- solve_build_cmd(x = x, reduce_var = reduce_var, rform = TRUE)
  
  # From yac_expr()
  z <- yac_core(cmd)
  z <- z[2L]
  z <- strip_start_end_quotes(z)
  z <- parse(text = z, keep.source = FALSE)
  
  return(z)
}

