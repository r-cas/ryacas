.onLoad <- function(libname, pkgname){
  if (exists("ryacas_devel_use_devtools", envir = .GlobalEnv)) {
    cat("Initializing internal Yacas... ")  
    # Force initialize so that a new instance is created.
    # Especially useful during development with e.g. devtools::load_all().
    yacas_init_force()
    cat("Done.\n")
  }
}

