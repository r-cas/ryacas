# Used for helping write issues for yacas
# yi = yacas issue
yi <- function(x) {
  cat("In> ", x, "\n", sep = "")
  
  out <- yac_str(x)
  cat("Out> ", out, "\n", sep = "")
  
  return(invisible(out))
}
