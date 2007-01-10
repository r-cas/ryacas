
.First.lib <- function(lib, pkg, ...) {
   if (.Platform$OS.type != "windows") return()

   if (!file.exists(yacasFile("yacas.exe")) ||  
       !file.exists(yacasFile("scripts.dat"))) {
	cat(yacasFile("yacas.exe"), "\n   or", yacasFile("scripts.dat"), 
           "\n   not found.\n")
        cat("Run yacasInstall() without arguments to install yacas.\n")
   }
   invisible()
}
