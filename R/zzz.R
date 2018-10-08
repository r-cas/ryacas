.onLoad <- function(libname, pkgname){
  if (exists("ryacas_devel_use_devtools", envir = .GlobalEnv)) {
    packageStartupMessage("Initialising internal Yacas... ", appendLF = FALSE)  
    # Force initialise so that a new instance is created.
    # Especially useful during development with e.g. devtools::load_all().
    yacas_init_force()
    packageStartupMessage("Done.", appendLF = TRUE)
  }
}

