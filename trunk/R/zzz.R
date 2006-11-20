
.First.lib <- function(lib, pkg, ...) {
   if (.Platform$OS.type != "windows") return()
   # library.dynam("Ryacas", pkg, lib)
   chk <- yacasCheck()
   if (is.na(chk)) return()
   if (chk == 1) cat("Wrong version of yacas installed.\n")
   if (chk == -1) cat("yacas not found.\n")
   if (Sys.getenv("YACAS_INVOKE_STRING") == "") {
         if (is.na(chk)) return()
         if (chk != 0) {
              urlbase <- "http://ryacas.googlecode.com/svn/trunk/inst/yacdir"
              cat("Run yacasInstall() without arguments to install yacas\n")
	      cat(" from", file.path(urlbase, "yacas.exe"), "\n")
	      cat(" and", file.path(urlbase, "scripts.dat"), "\n")
	      cat(" to", yacasFile("yacas.exe"), "and", 
                yacasFile("scripts.dat"), "\n")
          }
   }
   invisible()
}
