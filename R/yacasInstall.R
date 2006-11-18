yacasInstall <- function(showonly = FALSE, ...) {
   stopifnot(.Platform$OS.type == "windows")
   urlbase <- "http://ryacas.googlecode.com/svn/trunk/inst/yacdir"
   yacdir <- system.file(package = "Ryacas", "yacdir")
   files <- c("scripts.dat", "yacas.exe")
   lapply(files, function(f) if (showonly)
      cat("Download", file.path(urlbase, f), "\nto", file.path(yacdir, f), "\n")
   else
      download.file(file.path(urlbase, f), file.path(yacdir, f), 
         mode = "wb", ...) 
   )
}
